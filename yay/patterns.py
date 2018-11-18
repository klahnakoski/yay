# encoding: utf-8
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Author: Kyle Lahnakoski (kyle@lahnakoski.com)
#

from __future__ import absolute_import
from __future__ import division
from __future__ import unicode_literals

from collections import Mapping

from mo_dots import Null
from mo_future import transpose, items
from mo_logs import Log
from mo_logs.strings import quote

from yay.util import LimitUsage
from yay.parsers import (
    LiteralParser,
    CharactersParser,
    OneOrMoreParser,
    ConcatParser,
    ForwardParser,
    OrParser,
    ForwardPlaceholder)


class Pattern(object):
    """
    Patterns DECLARE A GRAMMER, AND THEY ARE USED TO SPAWN PARSERS THAT TRACK THE CURRENT MATCHING
    """

    def __init__(self, parser):
        self._parser = parser
        self.parse_actions = []
        self.post = lambda x: x

    def consume(self, character):
        """
        :param character:  CHARACTER TO CONSUME
        :return: tuple(MATCHES, STILL-VALID PARSERS)
        """
        return Null, Null

    def addParseAction(self, action):
        self.parse_actions.append(action)

    def _apply_actions(self, match):
        for a in self.parse_actions:
            match = a(match)
        return match

    def parser(self, lhs, character, position):
        """
        :param lhs: CUURENT PARSE TREE
        :param character: NOT CONSUMED, JUST A HELPFUL TO DETERMINE IF A PARSER SHOULD BE RETURNED
        :return:
        """
        raise NotImplementedError()

    def new_parser(self, position):
        """
        RETURN A NEW PARSER WITH ALL THE EXPECTATIONS IT HAS
        :return:
        """
        return self._parser(self, position)

    def post_process(self, func):
        """
        :param func: ACCEPTS A Match AND RETURNS SOME OTHER JSON-IZABLE OBJECT
        :return: self FOR CHAINING
        """
        self.post = func
        return self

    def accept_rhs(self, rhs):
        return self == rhs


class Literal(Pattern):
    """
    MATCH A SPECIFIC UNICODE STRING
    """

    def __init__(self, value):
        Pattern.__init__(self, LiteralParser)
        self.value = value

    def __unicode__(self):
        return "Literal(" + self.value + ")"

    def __str__(self):
        return quote(self.value)

    def __eq__(self, other):
        return isinstance(other, Literal) and self.value == other.value

    def parser(self, lhs, character, position):
        if not lhs and self.value[0] == character:
            return [LiteralParser(self, position)]
        else:
            return []

    def accept_rhs(self, rhs):
        return self == rhs


class Characters(Pattern):
    """
    MATCH SPECIFIC SET OF UNICODE CHARACTERS
    """

    def __init__(self, allowed_chars):
        Pattern.__init__(self, CharactersParser)
        self.allowed_chars = allowed_chars

    def parser(self, lhs, character, position):
        if not lhs and character in self.allowed_chars:
            return [CharactersParser(self, position)]
        return []

    def __str__(self):
        return str("[" + self.allowed_chars + "]")


class OneOrMore(Pattern):
    def __init__(self, sub_pattern):
        Pattern.__init__(self, OneOrMoreParser)
        self.sub_pattern = sub_pattern

    def parser(self, lhs, character, position):

        tail = self.sub_pattern.parser(None, character, position)
        if not tail:
            return []

        if not lhs:
            return [OneOrMoreParser(self, position, [], t) for t in tail]

        output = []
        parent_token, right_token = None, lhs
        while right_token:
            if right_token.pattern == self:
                output.extend(
                    OneOrMoreParser(self, position, right_token.sequence, t)
                    for t in tail
                )
            parent_token, right_token = right_token, right_token.right_token
        return output

    def __str__(self):
        return str("*(") + str(self.sub_pattern) + str(")")


class Concat(Pattern):
    def __init__(self, sub_patterns):
        """
        :param sub_patterns: LIST OF {name: pattern} PAIRS; name==None FOR NO NAME
        """
        Pattern.__init__(self, ConcatParser)

        def parse_pair(p):
            if isinstance(p, Mapping):
                return items(p)[0]
            elif isinstance(p, Pattern):
                return None, p
            else:
                Log.error("Expecting pattern, or dict like {'name': pattern}")

        self.names, self.sub_patterns = transpose(
            *(parse_pair(p) for p in sub_patterns)
        )

    def parser(self, lhs, character, position):
        if not lhs:
            first_patterns = self.sub_patterns[0].parser(lhs, character, position)
            return [ConcatParser(self, position, [], 0, f) for f in first_patterns]
        elif self.sub_patterns[0] in lhs.patterns:
            next_parsers = self.sub_patterns[1].parser(None, character, position)
            return [ConcatParser(self, position, [lhs], 1, p) for p in next_parsers]
        else:
            return []

    def accept_rhs(self, rhs):
        last_pattern = self.sub_patterns[-1]
        return last_pattern == rhs

    def __str__(self):
        return str("(") + (str(" + ").join(map(str, self.sub_patterns))) + str(")")


class Or(Pattern):
    def __init__(self, sub_patterns):
        Pattern.__init__(self, OrParser)
        self.sub_patterns = sub_patterns

    def parser(self, lhs, character, position):
        if not lhs:
            return [
                OrParser(
                    self,
                    position,
                    [
                        s
                        for p in self.sub_patterns
                        for s in p.parser(lhs, character, position)
                    ],
                )
            ]

        candidates = []
        for p in self.sub_patterns:
            candidates.extend(p.parser(lhs, character, position))

        if not candidates:
            return []
        else:
            return [OrParser(self, position, candidates)]

    def accept_rhs(self, rhs):
        return any(p.accept_rhs(rhs) for p in self.sub_patterns)

    def __str__(self):
        return str("(") + str(") | (").join(map(str, self.sub_patterns)) + str(")")


class Forward(Pattern):
    def __init__(self, name):
        Pattern.__init__(self, ForwardPlaceholder)
        self.name = name
        self.sub_pattern = Null
        self.in_use = set()  # STOP RECURSIVE LOOPS

    def __lshift__(self, sub_pattern):
        if self.sub_pattern:
            Log.error("expecting assignment to Forward pattern just once")
        else:
            self.sub_pattern = sub_pattern

    def parser(self, lhs, character, position):

        acc = []
        right_token = lhs
        while right_token:
            if self in right_token.patterns:
                acc.extend(
                    self.sub_pattern.parser(
                        right_token.right_token, character, position
                    )
                )
            right_token = right_token.right_token

        if position in self.in_use:
            return [ForwardPlaceholder(self, position, lhs)]

        with self.at(position):
            return [
                ForwardParser(self, position, p)
                for p in self.sub_pattern.parser(lhs, character, position)
            ]

    def at(self, position):
        return LimitUsage(self.in_use, position)

    def __str__(self):
        return str("<") + str(self.name) + str(">")

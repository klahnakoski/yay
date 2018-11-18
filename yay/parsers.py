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

from mo_dots import Null
from mo_future import unichr
from mo_logs import Log
from mo_logs.strings import quote

from yay.matches import LiteralMatch, CharacterMatch, OneOrMoreMatch, ConcatMatch


class Parser(object):
    def __init__(self, pattern, start_pos):
        self.start = start_pos
        self.pattern = pattern

    def __str__(self):
        return str("Parse(") + str(self.pattern) + str(")")


class LiteralParser(Parser):
    def __init__(self, pattern, start_pos, index=0):
        Parser.__init__(self, pattern, start_pos)
        self.index = index
        self.start_pos = -1

    def consume(self, character, position):
        if self.index == 0:
            self.start_pos = position
        if character == self.pattern.value[self.index]:
            self.index += 1
            if self.index == len(self.pattern.value):
                return (
                    [
                        LiteralMatch(
                            self.pattern, self.start_pos, self.start_pos + self.index
                        )
                    ],
                    [],
                )
            else:
                return Null, [self]
        else:
            self.index += 1
            return Null, Null

    def __str__(self):
        return (
            str("Parse ")
            + quote(self.pattern.value[self.index])
            + str(" in ")
            + quote(self.pattern.value)
        )


NO_CHAR = unichr(0)


class CharactersParser(Parser):
    def __init__(self, pattern, start_pos):
        Parser.__init__(self, pattern, start_pos)
        self.character = NO_CHAR
        self.position = -1

    def consume(self, character, position):
        if character in self.pattern.allowed_chars:
            return [CharacterMatch(self.pattern, character, position)], Null
        else:
            return Null, Null

    def __str__(self):
        return str("one of ") + quote(self.pattern)


class OneOrMoreParser(Parser):
    def __init__(self, pattern, start_pos, prefix=None, sub_parser=None):
        Parser.__init__(self, pattern, start_pos)
        self.prefix = prefix if prefix is not None else []
        self.curr_sub_parser = (
            sub_parser
            if sub_parser is not None
            else pattern.sub_pattern.new_parser(start_pos)
        )

    def consume(self, character, position):
        total_matches = []
        next_parsers = []

        matches, new_parsers = self.curr_sub_parser.consume(character, position)

        for match in matches:
            # FOR EACH MATCH, THIS IS DONE
            sequence = self.prefix + [match]
            total_matches.append(OneOrMoreMatch(self.pattern, sequence))
            # ...OR WE CAN ATTEMPT ANOTHER SUB_PATTERN MATCH
            next_sub_parser = self.pattern.sub_pattern.new_parser(position)
            next_parsers.append(
                OneOrMoreParser(self.pattern, position, sequence, next_sub_parser)
            )

        for p in new_parsers:
            next_parsers.append(OneOrMoreParser(self.pattern, position, self.prefix, p))

        return total_matches, next_parsers

    def __str__(self):
        return str("(") + str(self.curr_sub_parser) + str(") in ") + str(self.pattern)


class ConcatParser(Parser):
    def __init__(
        self, pattern, start_pos, prefix_data=None, curr_index=None, curr_parser=None
    ):
        Parser.__init__(self, pattern, start_pos)
        self.prefix = prefix_data or []
        if curr_parser is not None:
            self.curr_index = curr_index
            self.curr_parser = curr_parser
        else:
            self.curr_index = 0
            self.curr_parser = pattern.sub_patterns[0].new_parser(start_pos)

    def consume(self, character, position):
        next_parsers = []
        sub_matches, new_parsers = self.curr_parser.consume(character, position)

        if self.curr_index == len(self.pattern.sub_patterns) - 1:
            # THE LAST MATCH WILl RETURN THE CONCATENATED MATCH, AND ANY OTHER STILL-VALID PARSERS
            total_matches = []
            for m in sub_matches:
                total_matches.append(ConcatMatch(self.pattern, self.prefix + [m]))
            for p in new_parsers:
                next_parser = ConcatParser(
                    self.pattern, self.start, self.prefix, self.curr_index, p
                )
                next_parsers.append(next_parser)

            return total_matches, next_parsers
        else:
            # EVERY MATCH WILL TRIGGER A COPY OF self, ADVANCED TO THE NEXT IN THE SERIES
            for m in sub_matches:
                next_parser = ConcatParser(
                    self.pattern,
                    self.start,
                    self.prefix + [m],
                    self.curr_index + 1,
                    self.pattern.sub_patterns[self.curr_index + 1].new_parser(
                        position + 1
                    ),
                )
                next_parsers.append(next_parser)
            for p in new_parsers:
                next_parser = ConcatParser(
                    self.pattern, self.start, self.prefix, self.curr_index, p
                )
                next_parsers.append(next_parser)
            return Null, next_parsers

    def __str__(self):
        return (
            str("(")
            + str(self.curr_parser)
            + str(") ")
            + str(self.curr_index)
            + (" of ")
            + str(len(self.pattern.sub_patterns))
            + (" in ")
            + str(self.pattern)
        )


class OrParser(Parser):
    def __init__(self, pattern, start_pos, sub_parsers=None):
        Parser.__init__(self, pattern, start_pos)
        if sub_parsers is None:
            self.sub_parsers = [
                p.new_parser(start_pos) for p in self.pattern.sub_patterns
            ]
        else:
            self.sub_parsers = sub_parsers

    def consume(self, character, position):
        total_matches = []
        next_parsers = []
        for p in self.sub_parsers:
            matches, new_parsers = p.consume(character, position)
            for m in matches:
                m.patterns.append(self.pattern)
            total_matches.extend(matches)
            next_parsers.extend(new_parsers)
        if next_parsers:
            next_parsers = [OrParser(self.pattern, self.start, next_parsers)]
        return total_matches, next_parsers


class ForwardParser(Parser):
    def __init__(self, pattern, start_pos, sub_parser):
        Parser.__init__(self, pattern, start_pos)
        self.sub_parser = sub_parser

    def consume(self, character, position):
        if self.start in self.pattern.in_use:
            return Null, Null

        with self.pattern.at(self.start):
            # MATCHES EXIST ANY MANY LEVELS OF PARSERS, EACH MATCH SHOULD LIST THE HIERARCHY OF PARSERS THAT MATCHED IT
            total_parsers = []
            new_matches, new_parsers = self.sub_parser.consume(character, position)
            for mm in new_matches:
                mm.patterns.append(self.pattern)
                total_parsers.append(ForwardPlaceholder(self.pattern, self.start, mm))

            total_parsers.extend(
                ForwardParser(self.pattern, self.start, pp) for pp in new_parsers
            )

            return new_matches, total_parsers


class ForwardPlaceholder(Parser):
    def __init__(self, pattern, start_pos, lhs=None):
        Parser.__init__(self, pattern, start_pos)
        self.lhs = lhs  # None IMPLIES FRESH PLACEHOLDER

    def consume(self, character, position):
        # THIS PARSER HAS NOT BEEN INITAILIZED (TO PREVENT RECURSIVE LOOPS)
        if self.lhs is None:
            start = self.start = position
        else:
            start = self.start = self.lhs.start

        if self.start in self.pattern.in_use:
            return Null, Null

        return ForwardParser(
            self.pattern,
            start,
            self.pattern.sub_pattern.parser(self.lhs, character, position)[0],
        ).consume(character, position)

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

from mo_future import unichr, transpose, items
from mo_dots import Null, coalesce
from mo_future import text_type
from mo_logs import Log

whitespace = ' \t\n\r\v\f'
lowercase = 'abcdefghijklmnopqrstuvwxyz'
uppercase = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
letters = lowercase + uppercase
ascii_lowercase = lowercase
ascii_uppercase = uppercase
ascii_letters = ascii_lowercase + ascii_uppercase
digits = '0123456789'
hexdigits = digits + 'abcdef' + 'ABCDEF'
octdigits = '01234567'
punctuation = """!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~"""
printable = digits + letters + punctuation + whitespace


class Pattern(object):
    """
    Patterns DECLARE A GRAMMER, AND THEY ARE USED TO SPAWN PARSERS THAT TRACK THE CURRENT MATCHING
    """

    def __init__(self, parser):
        self._parser = parser
        self.parse_actions = []

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

    def parser(self, lhs, character):
        """
        :param lhs:
        :param character: NOT CONSUMED, JUST A HELPFUL TO DETERMINE IF A PARSER SHOULD BE RETURNED
        :return:
        """
        raise NotImplementedError()

    def new_parser(self):
        """
        RETURN A NEW PARSER WITH ALL THE EXPECTATIONS IT HAS
        :return:
        """
        return self._parser(self)

    def accept_rhs(self, rhs):
        return self == rhs

    def __str__(self):
        return text_type(self).encode('utf8')


class Parser(object):

    def __init__(self, pattern):
        self.pattern=pattern

    def __str__(self):
        return text_type(self).encode('utf8')


class Match(object):

    def __init__(self, pattern):
        self.pattern=pattern


class Literal(Pattern):
    """
    MATCH A SPECIFIC UNICODE STRING
    """

    def __init__(self, value):
        Pattern.__init__(self, LiteralParser)
        self.value = value

    def __unicode__(self):
        return "Literal(" + self.value + ")"

    def __eq__(self, other):
        return isinstance(other, Literal) and self.value == other.value

    def parser(self, lhs, character):
        if not lhs and self.value[0] == character:
            return [LiteralParser(self)]
        else:
            return []

    def accept_rhs(self, rhs):
        return self == rhs


class LiteralParser(Parser):
    def __init__(self, pattern, index=0):
        Parser.__init__(self, pattern)
        self.index = index
        self.start_pos = -1

    def consume(self, character, position):
        if self.index == 0:
            self.start_pos = position
        if character == self.pattern.value[self.index]:
            self.index += 1
            if self.index == len(self.pattern.value):
                return [LiteralMatch(self.pattern, self.start_pos, self.start_pos + self.index)], []
            else:
                return Null, [self]
        else:
            self.index += 1
            return Null, Null


class LiteralMatch(Match):

    def __init__(self, pattern, start, stop):
        Match.__init__(self, pattern)
        self.start = start
        self.stop = stop

    def __data__(self):
        return {
            "start": self.start,
            "stop": self.stop,
            "literal": self.pattern.value
        }


class Characters(Pattern):
    """
    MATCH SPECIFIC SET OF UNICODE CHARACTERS
    """

    def __init__(self, allowed_chars):
        Pattern.__init__(self, CharactersParser)
        self.allowed_chars = allowed_chars

    def parser(self, lhs, character):
        if not lhs and character in self.allowed_chars:
            return [CharactersParser(self)]
        return []



NO_CHAR = unichr(0)


class CharactersParser(Parser):
    def __init__(self, pattern):
        Parser.__init__(self, pattern)
        self.character = NO_CHAR
        self.position = -1

    def consume(self, character, position):
        if character in self.pattern.allowed_chars:
            return [CharacterMatch(self.pattern, character, position)], Null
        else:
            return Null, Null


class CharacterMatch(Match):
    def __init__(self, pattern, character, position):
        Match.__init__(self, pattern)
        self.character = character
        self.position = position

    @property
    def start(self):
        return self.position

    @property
    def stop(self):
        return self.position+1

    def __data__(self):
        return {
            "start": self.position,
            "stop": self.position+1,
            "data": self.character
        }

    @property
    def right_token(self):
        return None


class OneOrMore(Pattern):
    def __init__(self, sub_pattern):
        Pattern.__init__(self, OneOrMoreParser)
        self.sub_pattern = sub_pattern

    def parser(self, lhs, character):

        tail = self.sub_pattern.parser(None, character)
        if not tail:
            return []

        if not lhs:
            return [OneOrMoreParser(self, [], t) for t in tail]

        output = []
        parent_token, right_token = None, lhs
        while right_token:
            if right_token.pattern == self:
                output.extend(OneOrMoreParser(self, right_token.sequence, t) for t in tail)
            parent_token, right_token = right_token, right_token.right_token
        return output


class OneOrMoreParser(Parser):
    def __init__(self, pattern, prefix=None, sub_parser=None):
        Parser.__init__(self, pattern)
        self.prefix = prefix if prefix is not None else []
        self.curr_sub_parser = sub_parser if sub_parser is not None else pattern.sub_pattern.new_parser()

    def consume(self, character, position):
        total_matches = []
        next_parsers = []

        matches, new_parsers = self.curr_sub_parser.consume(character, position)

        for match in matches:
            # FOR EACH MATCH, THIS IS DONE
            sequence = self.prefix + [match]
            total_matches.append(OneOrMoreMatch(self.pattern, sequence))
            # ...OR WE CAN ATTEMPT ANOTHER SUB_PATTERN MATCH
            next_sub_parser = self.pattern.sub_pattern.new_parser()
            next_parsers.append(OneOrMoreParser(self.pattern, sequence, next_sub_parser))

        for p in new_parsers:
            next_parsers.append(OneOrMoreParser(self.pattern, self.prefix, p))

        return total_matches, next_parsers


class OneOrMoreMatch(Match):

    def __init__(self, pattern, sequence):
        Match.__init__(self, pattern)
        self.sequence = sequence

    def __data__(self):
        return {
            "start":self.sequence[0].start,
            "stop":self.sequence[-1].stop,
            "sequence": [d.__data__() for d in self.sequence]
        }

    @property
    def start(self):
        return self.sequence[0].start

    @property
    def stop(self):
        return self.sequence[-1].stop

    @property
    def right_token(self):
        return self.sequence[-1]


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
        self.names, self.sub_patterns = transpose(*(parse_pair(p) for p in sub_patterns))

    def parser(self, lhs, character):
        acc = []
        first_patterns = self.sub_patterns[0].parser(lhs, character)

        if not lhs:
            return [ConcatParser(self, [], 0, f) for f in first_patterns]

        parent_token = Null
        right_token = lhs
        while right_token:
            if parent_token.pattern.accept_rhs(self):
                acc.extend(first_patterns.parser(right_token, character))
            right_token = right_token.right_token
        return acc

    def accept_rhs(self, rhs):
        last_pattern = self.sub_patterns[-1]
        return last_pattern == rhs


class ConcatParser(Parser):
    def __init__(self, pattern, prefix_data=None, curr_index=None, curr_parser=None):
        Parser.__init__(self, pattern)
        self.prefix = prefix_data or []
        if curr_parser is not None:
            self.curr_index = curr_index
            self.curr_parser = curr_parser
        else:
            self.curr_index = 0
            self.curr_parser = pattern.sub_patterns[0].new_parser()

    def consume(self, character, position):
        next_parsers = []
        sub_matches, new_parsers = self.curr_parser.consume(character, position)

        if self.curr_index == len(self.pattern.sub_patterns)-1:
            # THE LAST MATCH WILl RETURN THE CONCATENATED MATCH, AND ANY OTHER STILL-VALID PARSERS
            total_matches = []
            for m in sub_matches:
                total_matches.append(ConcatMatch(
                    self.pattern,
                    self.prefix+[m]
                ))
            for p in new_parsers:
                next_parser = ConcatParser(
                    self.pattern,
                    self.prefix,
                    self.curr_index,
                    p
                )
                next_parsers.append(next_parser)

            return total_matches, next_parsers
        else:
            # EVERY MATCH WILL MAKE A COPY OF self, ADVANCED TO THE NEXT IN THE SERIES
            for m in sub_matches:
                next_parser = ConcatParser(
                    self.pattern,
                    self.prefix + [m],
                    self.curr_index + 1,
                    self.pattern.sub_patterns[self.curr_index + 1].new_parser()
                )
                next_parsers.append(next_parser)
            for p in new_parsers:
                next_parser = ConcatParser(
                    self.pattern,
                    self.prefix,
                    self.curr_index,
                    p
                )
                next_parsers.append(next_parser)
            return Null, next_parsers


class ConcatMatch(Match):

    def __init__(self, pattern, sequence):
        Match.__init__(self, pattern)
        self.sequence = sequence

    def __data__(self):
        seq = [s.__data__() for s in self.sequence]
        return {
            "start": self.sequence[0].start,
            "stop": self.sequence[-1].stop,
            "sequence": seq,
            "data": {
                n: s
                for n, s in zip(self.pattern.names, seq)
                if n is not None
            }
        }

    @property
    def start(self):
        return self.sequence[0].start

    @property
    def stop(self):
        return self.sequence[-1].stop

    @property
    def right_token(self):
        return self.sequence[-1]


class Or(Pattern):
    def __init__(self, sub_patterns):
        Pattern.__init__(self, OrParser)
        self.sub_patterns = sub_patterns

    def parser(self, lhs, character):
        if not lhs:
            return [OrParser(self, [s for p in self.sub_patterns for s in p.parser(lhs, character)])]

        if lhs.pattern != self:
            return []

        rhs = lhs.right_token
        candidates = []
        for p in self.sub_patterns:
            candidates.extend(p.parser(rhs, character))
        return [OrParser(self, candidates)]

    def accept_rhs(self, rhs):
        return any(p.accept_rhs(rhs) for p in self.sub_patterns)


class OrParser(Parser):

    def __init__(self, pattern, sub_parsers=None):
        Parser.__init__(self, pattern)
        if sub_parsers is None:
            self.sub_parsers = [p.new_parser() for p in self.pattern.sub_patterns]
        else:
            self.sub_parsers = sub_parsers

    def consume(self, character, position):
        total_matches = []
        next_parsers = []
        for p in self.sub_parsers:
            matches, new_parsers = p.consume(character, position)
            total_matches.extend(OrMatch(self.pattern, m) for m in matches)
            next_parsers.extend(OrParser(self.pattern, [p]) for p in new_parsers)

        return total_matches, next_parsers


class OrMatch(Match):

    def __init__(self, pattern, sub_match):
        Match.__init__(self, pattern)
        self.sub_match = sub_match


    @property
    def start(self):
        return self.sub_match.start

    @property
    def stop(self):
        return self.sub_match.stop

    @property
    def right_token(self):
        return self.sub_match


class Forward(Pattern):

    def __init__(self):
        Pattern.__init__(self, ForwardParser)
        self.sub_pattern = Null
        self.in_use = False

    def __lshift__(self, sub_pattern):
        if self.sub_pattern:
            Log.error("expecting assignment to Forward pattern just once")
        else:
            self.sub_pattern = sub_pattern

    def parser(self, lhs, character):

        acc = []
        right_token = lhs
        while right_token:
            if right_token.pattern == self:
                acc.extend(self.sub_pattern.parser(right_token.right_token, character))
            right_token = right_token.right_token

        if self.in_use:
            return [ForwardParser(self)]

        with self:
            return [ForwardParser(self, p) for p in self.sub_pattern.parser(lhs, character)]

    def __enter__(self):
        if self.in_use:
            Log.error("not allowed")
        self.in_use = True

    def __exit__(self, a, b, c):
        self.in_use = False


class ForwardParser(Parser):

    def __init__(self, pattern, sub_parser=None, matches=None):
        Parser.__init__(self, pattern)
        self.sub_parser = sub_parser
        self.sub_matches = matches or []

    def consume(self, character, position):
        if self.sub_parser is None:
            return Null, Null

        new_sub_parsers = [self.sub_parser]
        for m in self.sub_matches:
            new_sub_parsers.extend(self.pattern.sub_pattern.parser(m, character))

        new_matches = []
        new_parsers = []
        for s in new_sub_parsers:
            m, p = s.consume(character, position)
            new_matches.extend(ForwardMatch(self.pattern, mm) for mm in m)
            new_parsers.extend(ForwardParser(self.pattern, pp, m) for pp in p)

        return new_matches, new_parsers


class ForwardMatch(Match):

    def __init__(self, pattern, sub_match):
        Match.__init__(self, pattern)
        self.sub_match = sub_match

    @property
    def start(self):
        return self.sub_match.start

    @property
    def stop(self):
        return self.sub_match.stop

    @property
    def right_token(self):
        return self.sub_match



# SOME SHORTCUTS
def Word(characters):
    return OneOrMore(Characters(characters))


def Whitespace():
    return OneOrMore(Characters(whitespace))


def parse(pattern, data):
    # ONLY RETURN MATCHES THAT CONSUME ALL data
    parsers = pattern.parser(None, data[0])
    matches = []
    for i, d in enumerate(data):
        next_parsers = []
        matches = []
        for p in parsers:
            new_matches, new_parsers = p.consume(d, i)
            matches.extend(new_matches)
            next_parsers.extend(new_parsers)
        parsers = next_parsers

    return [m.__data__() for m in matches]



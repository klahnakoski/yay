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
            # EVERY MATCH WILL MAKE A COPY OF self, ADVANCED TO THE NEXT IN THE SERIES
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
            if new_parsers:
                next_parsers.append(OrParser(self.pattern, self.start, new_parsers))

        return total_matches, next_parsers


class ForwardParser(Parser):
    def __init__(self, pattern, start_pos, sub_parser=None, matches=None):
        Parser.__init__(self, pattern, start_pos)
        self.sub_parser = sub_parser
        self.sub_matches = matches or []

    def consume(self, character, position):
        if self.sub_parser is None:
            if position in self.pattern.in_use:
                return Null, Null

            self.sub_parser = self.pattern.sub_pattern.new_parser(position)

        with self.pattern.at(self.start):
            new_sub_parsers = [self.sub_parser]
            for m in self.sub_matches:
                new_sub_parsers.extend(
                    self.pattern.sub_pattern.parser(m, character, position)
                )
            # MATCHES EXIST ANY MANY LEVELS OF PARSERS, EACH MATCH SHOULD LIST THE HIERARCHY OF PARSERS THAT MATCHED IT
            new_matches = []
            new_parsers = []
            for s in new_sub_parsers:
                m, p = s.consume(character, position)
                for mm in m:
                    mm.patterns.append(self.pattern)
                new_matches.extend(m)
                new_parsers.extend(
                    ForwardParser(self.pattern, self.start, pp, m) for pp in p
                )

            return new_matches, new_parsers

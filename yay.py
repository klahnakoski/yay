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
    def __init__(self):
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

    def __str__(self):
        return unicode(self).encode('utf8')


class Parser(object):
    def __str__(self):
        return unicode(self).encode('utf8')


class Literal(Pattern):
    """
    MATCH A SPECIFIC UNICODE STRING
    """

    def __init__(self, value):
        Pattern.__init__(self)
        self.value = value

    def parser(self, start_pos):
        return LiteralParser(self, start_pos)

    def __unicode__(self):
        return "Literal(" + self.value + ")"


class LiteralParser(Parser):
    def __init__(self, literal_pattern, start_pos):
        self.pattern = literal_pattern
        self.position = 0
        self.start_pos = start_pos

    def consume(self, character):
        if character == self.pattern.value[self.position]:
            self.position += 1
            if self.position == len(self.pattern.value):
                return [Match(self.start_pos, self.start_pos + self.position, self.pattern.value)], []
            else:
                return Null, [self]
        else:
            self.position += 1
            return Null, Null


class Characters(Pattern):
    """
    MATCH SPECIFIC SET OF UNICODE CHARACTERS
    """

    def __init__(self, allowed_chars):
        Pattern.__init__(self)
        self.allowed_chars = allowed_chars

    def parser(self, start_pos):
        return CharactersParser(self, start_pos)


class CharactersParser(Parser):
    def __init__(self, word_pattern, start_pos):
        self.pattern = word_pattern
        self.start_pos = start_pos

    def consume(self, character):
        if character in self.pattern.allowed_chars:
            return [Match(self.start_pos, self.start_pos + 1, character)], Null
        else:
            return Null, Null


class OneOrMore(Pattern):
    def __init__(self, pattern):
        Pattern.__init__(self)
        self.sub_pattern = pattern

    def parser(self, start_pos):
        return OneOrMoreParser(self, start_pos, start_pos, [])


class OneOrMoreParser(Parser):
    def __init__(self, pattern, start_pos, stop_pos, prefix_data):
        self.pattern = pattern
        self.start_pos = start_pos
        self.sub_parser = [pattern.sub_pattern.parser(start_pos)]
        self.stop_pos = stop_pos
        self.prefix_data = prefix_data

    def consume(self, character):
        self.stop_pos += 1
        total_matches = []
        next_parsers = []

        for p in self.sub_parser:
            matches, new_parsers = p.consume(character)

            for match in matches:
                data = self.prefix_data + [match.data]
                total_matches.append(Match(self.start_pos, self.stop_pos, data))
                next_parser = OneOrMoreParser(self.pattern, self.start_pos, self.stop_pos, data)
                next_parsers.append(next_parser)

            if new_parsers:
                self.sub_parser = new_parsers
                next_parsers.append(self)

        return total_matches, next_parsers


class Or(Pattern):
    def __init__(self, sub_patterns):
        Pattern.__init__(self)
        self.sub_patterns = sub_patterns

    def parser(self, start_pos):
        return OrParser(self, start_pos)


class OrParser(Parser):

    def __init__(self, pattern, start_pos):
        self.pattern = pattern
        self.start_pos = start_pos
        self.stop_pos = start_pos
        self.sub_parsers = [p.parser(start_pos) for p in self.pattern.sub_patterns]

    def consume(self, character):
        self.stop_pos += 1
        total_matches = []
        next_parsers = []
        for p in self.sub_parsers:
            matches, new_parsers = p.consume(character)
            total_matches.extend(matches)
            next_parsers.extend(new_parsers)
        self.sub_parsers = next_parsers
        return total_matches, next_parsers


class Word(Pattern):

    def __init__(self, characters):
        Pattern.__init__(self)
        self.characters = characters

    def parser(self, start_pos):
        return OneOrMore(Characters(self.characters)).parser(start_pos)


class White(Pattern):
    def __init__(self):
        Pattern.__init__(self)
        pass

    def parser(self, start_pos):
        return WhiteParser(start_pos)


class WhiteParser(Parser):
    """
    CONSUME ZERO, OR MORE WHITESPACE CHARACTERS
    """

    def __init__(self, start_pos):
        self.start_pos = start_pos
        self.stop_pos = start_pos

    def consume(self, character):
        self.stop_pos += 1
        if character in whitespace:
            return [Match(start=self.start_pos, stop=self.stop_pos, data=" ")], [self]
        else:
            return Null, Null


class Concat(Pattern):

    def __init__(self, sub_patterns):
        """
        :param sub_patterns: LIST OF (name, pattern) PAIRS; name==None FOR NO NAME
        """
        Pattern.__init__(self)
        temp = [p if isinstance(p, tuple) else (Null, p) for p in sub_patterns]
        self.names, self.sub_patterns = zip(*temp)

    def parser(self, start_pos):
        return ConcatParser(self, start_pos, [])


class ConcatParser(Parser):
    def __init__(self, pattern, start_pos, prefix_data, curr_index=0, curr_parser=None):
        self.pattern = pattern
        self.start_pos = start_pos
        self.stop_pos = start_pos
        self.data = prefix_data
        self.curr_index = curr_index
        self.curr_parser = pattern.sub_patterns[0].parser(start_pos) if curr_parser is None else curr_parser

    def consume(self, character):
        self.stop_pos += 1
        next_parsers = []
        matches, new_parsers = self.curr_parser.consume(character)

        if self.curr_index == len(self.pattern.sub_patterns)-1:
            # THE LAST MATCH WILl RETURN THE CONCATENATED MATCH, AND ANY OTHER STILL-VALID PARSERS
            total_matches = []
            for m in matches:
                total_matches.append(Match(
                    self.start_pos,
                    self.stop_pos,
                    {k: v for k, v in zip(self.pattern.names, self.data + [m]) if k}
                ))
            for p in new_parsers:
                next_parser = ConcatParser(
                    self.pattern,
                    self.start_pos,
                    self.data,
                    self.curr_index,
                    p
                )
                next_parser.stop_pos = self.stop_pos
                next_parsers.append(next_parser)

            return total_matches, next_parsers
        else:
            # EVERY MATCH WILL MAKE A COPY OF self, ADVANCED TO THE NEXT IN THE SERIES
            for m in matches:
                next_parser = ConcatParser(
                    self.pattern,
                    self.start_pos,
                    self.data+[m],
                    self.curr_index + 1,
                    self.pattern.sub_patterns[self.curr_index + 1].parser(self.stop_pos)
                )
                next_parser.stop_pos = self.stop_pos
                next_parsers.append(next_parser)
            for p in new_parsers:
                next_parser = ConcatParser(
                    self.pattern,
                    self.start_pos,
                    self.data,
                    self.curr_index,
                    p
                )
                next_parser.stop_pos = self.stop_pos
                next_parsers.append(next_parser)
            return Null, next_parsers


class Match(object):
    def __init__(self, start, stop, data):
        self.start = start
        self.stop = stop
        self.data = data

    def __data__(self):
        return {
            "start": self.start,
            "stop": self.stop,
            "data": self.data
        }

def parse(pattern, data):
    matches = Null

    parsers = [pattern.parser(0)]
    for d in data:
        matches = Null
        if not parsers:
            Log.error("Can not match whole string")
        next_parsers = []
        for p in parsers:
            new_matches, new_parsers = p.consume(d)
            if new_matches:
                if matches:
                    matches.extend(new_matches)
                else:
                    matches = new_matches
            next_parsers.extend(new_parsers)
        parsers = next_parsers

    return matches

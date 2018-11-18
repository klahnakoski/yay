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


class Match(object):
    def __init__(self, pattern):
        self.patterns = [pattern]

    def __str__(self):
        raise NotImplementedError()


class LiteralMatch(Match):
    def __init__(self, pattern, start, stop):
        Match.__init__(self, pattern)
        self.start = start
        self.stop = stop

    def __data__(self):
        return {
            "start": self.start,
            "stop": self.stop,
            "literal": self.patterns[0].value,
        }

    def __str__(self):
        return str(self.patterns[0].value)


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
        return self.position + 1

    def __data__(self):
        return {
            "start": self.position,
            "stop": self.position + 1,
            "data": self.character,
        }

    @property
    def right_token(self):
        return None

    def __str__(self):
        return self.character


class OneOrMoreMatch(Match):
    def __init__(self, pattern, sequence):
        Match.__init__(self, pattern)
        self.sequence = sequence

    def __data__(self):
        return {
            "start": self.sequence[0].start,
            "stop": self.sequence[-1].stop,
            "sequence": [d.__data__() for d in self.sequence],
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

    def __str__(self):
        return "".join(str(s) for s in self.sequence)


class ConcatMatch(Match):
    def __init__(self, pattern, sequence):
        Match.__init__(self, pattern)
        self.sequence = sequence

    def __data__(self):
        seq = [s.__data__() for s in self.sequence if s is not None]
        return {
            "start": self.sequence[0].start,
            "stop": self.sequence[-1].stop,
            "sequence": seq,
            "data": {
                n: s
                for n, s in zip(self.patterns[0].names, seq)
                if n is not None and s is not None
            },
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

    def __str__(self):
        return "".join(str(s) for s in self.sequence)

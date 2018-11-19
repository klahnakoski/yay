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

from yay.matches import LiteralMatch, CharacterMatch, OneOrMoreMatch, ConcatMatch
from yay.patterns import Characters, OneOrMore
from yay.util import LimitUsage, whitespace


def text(match):
    return "".join(m.character for m in match.sequence)


def ignore(match):
    return None


# SOME SHORTCUTS
def Word(characters):
    return OneOrMore(Characters(characters)).post_process(text)


def Whitespace():
    return OneOrMore(Characters(whitespace)).post_process(ignore)


def parse(pattern, data):
    # ONLY RETURN MATCHES THAT CONSUME ALL data
    parsers = pattern.parser(None, data[0], 0)
    matches = []
    for i, d in enumerate(data):
        next_parsers = []
        matches = []
        for p in parsers:
            new_matches, new_parsers, expecting = p.consume(d, i)
            matches.extend(new_matches)
            next_parsers.extend(new_parsers)
        parsers = next_parsers

    return [m.__data__() for m in matches]

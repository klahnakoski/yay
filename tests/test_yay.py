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


from mo_testing.fuzzytestcase import FuzzyTestCase

from yay import Characters, Literal, parse, Or, letters, OneOrMore, Word, Concat, White


class Test_YAY(FuzzyTestCase):


    def test_one_char(self):
        pattern = Literal("a")
        result = parse(pattern, "a")
        self.assertEqual(result, [{"start": 0, "stop": 1, "data": "a"}])

    def test_literal(self):
        pattern = Literal("hello world")
        result = parse(pattern, "hello world")
        self.assertEqual(result, [{"start": 0, "stop": 11, "data": "hello world"}])

    def test_one_word(self):
        pattern = OneOrMore(Characters(letters))
        result = parse(pattern, "HelloWorld")
        self.assertEqual(
            result,
            [{"start": 0, "stop": 10, "data": ["H", "e", "l", "l", "o", "W", "o", "r", "l", "d"]}]
        )

    def test_or(self):
        pattern = Or([Literal("test"), Literal("stinker")])

        result = parse(pattern, "test")
        self.assertEqual(result, [{"start": 0, "stop": 4, "data": "test"}])

        result = parse(pattern, "stinker")
        self.assertEqual(result, [{"start": 0, "stop": 7, "data": "stinker"}])

        self.assertRaises(Exception, parse, pattern, "error")

    def test_concat(self):
        pattern = Concat([Literal("("), ("word", Word(letters)), Literal(")")])

        result = parse(pattern, "(hello)")
        self.assertEqual(
            result,
            [
                {"start": 0, "stop": 7, "data":
                    {"word": {"start": 1, "stop": 6, "data": ["h", "e", "l", "l", "o"]}}
                }
            ]
        )

    def test_whitespace(self):
        pattern = Concat([
            ("first", Word(letters)),
            ("rest", OneOrMore(Concat([
                White(),
                ("word", Word(letters))
            ])))
        ])
        #                        01234567890123
        result = parse(pattern, "this is a test")
        self.assertEqual(
            result,
            [
                {"start": 0, "stop": 14, "data": {
                    "first": {"start": 0, "stop": 4, "data": ["t", "h", "i", "s"]},
                    "rest": {"start": 4, "stop": 14, "data": [
                        {"word": {"start": 5, "stop": 7, "data": ["i", "s"]}},
                        {"word": {"start": 8, "stop": 9, "data": ["a"]}},
                        {"word": {"start": 10, "stop": 14, "data": ["t", "e", "s", "t"]}},
                    ]}
                }}
            ]
        )

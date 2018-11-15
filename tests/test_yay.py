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

from unittest import skip

from mo_testing.fuzzytestcase import FuzzyTestCase

from yay import Characters, Literal, parse, Or, letters, OneOrMore, Word, Concat, Whitespace, Forward, digits


class Test_YAY(FuzzyTestCase):

    def test_one_char(self):
        pattern = Literal("a")
        result = parse(pattern, "a")
        self.assertEqual(result, [{"start": 0, "stop": 1, "literal": "a"}])

    def test_literal(self):
        pattern = Literal("hello world")
        result = parse(pattern, "hello world")
        self.assertEqual(result, [{"start": 0, "stop": 11, "literal": "hello world"}])

    def test_characters(self):
        pattern = Characters(letters)
        result = parse(pattern, "H")
        self.assertEqual(
            result,
            [{"start": 0, "stop": 1, "data": "H"}]
        )

    def test_one_word(self):
        pattern = OneOrMore(Characters(letters))
        result = parse(pattern, "HelloWorld")
        self.assertEqual(
            result,
            [{
                "start": 0,
                "stop": 10,
                "sequence": [{"data": d} for d in "HelloWorld"]
            }]
        )

    def test_concat(self):
        pattern = Concat([Literal("("), Literal(")")])

        result = parse(pattern, "()")
        self.assertEqual(result, [{"start": 0, "stop": 2, "sequence": [{"literal": "("}, {"literal": ")"}]}])

    def test_concat_w_name(self):
        pattern = Concat([Literal("("), {"word": Word(letters)}, Literal(")")])

        result = parse(pattern, "(hello)")
        self.assertEqual(
            result,
            [{
                "start": 0,
                "stop": 7,
                "data": {"word": {"sequence": [{"data": w} for w in "hello"]}}
            }]
        )

    def test_or(self):
        pattern = Or([Literal("test"), Literal("stinker")])

        result = parse(pattern, "test")
        self.assertEqual(result, [{"start": 0, "stop": 4, "literal": "test"}])

        result = parse(pattern, "stinker")
        self.assertEqual(result, [{"start": 0, "stop": 7, "literal": "stinker"}])

    def test_whitespace(self):
        pattern = Concat([
            {"first": Word(letters)},
            {"rest": OneOrMore(Concat([
                Whitespace(),
                {"word": Word(letters)}
            ]))}
        ])
        #                        01234567890123
        result = parse(pattern, "this is a test")
        self.assertEqual(
            result,
            [
                {"start": 0, "stop": 14, "data": {
                    "first": {"start": 0, "stop": 4, "sequence": [{"data": w} for w in "this"]},
                    "rest": {"sequence": [
                        {"data": {"word": {"sequence": [{"data": w} for w in "is"]}}},
                        {"data": {"word": {"sequence": [{"data": w} for w in "a"]}}},
                        {"data": {"word": {"sequence": [{"data": w} for w in "test"]}}},
                    ]}
                }}
            ]
        )

    def test_forward(self):
        expr = Forward()
        sample = Or([
            OneOrMore(Characters(digits)),
            Concat([{"left": expr}, Whitespace(), {"op": Literal("+")}, Whitespace(), {"right": expr}]),
        ])
        expr << sample

        result = parse(expr, "2+4")
        expected = {"left": "2", "right": "4"}
        self.assertEqual(result, expected)

    def test_operators(self):
        expr = Forward()
        sample = Or([
            OneOrMore(Characters(digits)),
            Concat([{"left": expr}, Whitespace(), {"op": Literal("*")}, Whitespace(), {"right": expr}]),
            Concat([{"left": expr}, Whitespace(), {"op": Literal("+")}, Whitespace(), {"right": expr}]),
        ])
        expr << sample

        result = parse(expr, "2+4*3")
        expected = {"left": "2", "right": {"left": "4", "right": "3"}}
        self.assertEqual(result, expected)

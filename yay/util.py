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

from mo_logs import Log

whitespace = " \t\n\r\v\f"
lowercase = "abcdefghijklmnopqrstuvwxyz"
uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
letters = lowercase + uppercase
ascii_lowercase = lowercase
ascii_uppercase = uppercase
ascii_letters = ascii_lowercase + ascii_uppercase
digits = "0123456789"
hexdigits = digits + "abcdef" + "ABCDEF"
octdigits = "01234567"
punctuation = """!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~"""
printable = digits + letters + punctuation + whitespace


class LimitUsage(object):
    def __init__(self, in_use, position):
        self.in_use = in_use
        self.position = position

    def __enter__(self):
        if self.position in self.in_use:
            Log.error("not allowed")
        self.in_use.add(self.position)

    def __exit__(self, a, b, c):
        self.in_use.remove(self.position)

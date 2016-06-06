#!/usr/bin/env python
# The MIT License (MIT)
#
# Copyright (c) 2016 Fabio Calefato
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

"""
	 -	https://github.com/collab-uniba/

	Requires:
	-
"""

import getopt
import glob
import logging
import os
import re
import string
import sys

__script__ = 'collect-metrics.py'
__author__ = '@bateman'
__license__ = "MIT"
__date__ = '06-06-2016'
__version_info__ = (0, 0, 1)
__version__ = '.'.join(str(i) for i in __version_info__)
__home__ = 'https://github.com/collab-uniba/s'
__download__ = 'https://github.com/collab-uniba/.zip'

class ComputeMetrics(object):
    metric_files = None
    classification_res = None
    #['nb', 'knn', 'gpls', 'earth', 'glm', 'nnet', 'avNNet', 'pcaNNet', 'rbfDDA', 'mlp', 'mlpWeightDecay',
    # 'multinom', 'lda2', 'pda', 'fda', 'JRip', 'J48', 'LMT', 'rpart', 'svmLinear', 'svmRadial', 'rf',
    # 'treebag', 'gbm', 'AdaBoost.M1', 'gamboost', 'LogitBoost', 'C5.0', 'xgbTree']

    def __init__(self, infolder, outfile, ext, sep):
        self.log = logging.getLogger('ComputeMetrics script')

        self.infolder = infolder
        self.outfile = outfile
        self.sep = sep
        self.ext = ext

        self.metric_files = list()
        self.classification_res = dict()

    def main(self):
        self.__getfiles()
        for mf in self.metric_files:
            model = string.split(mf, sep=".")[0]
            fcontent = self.__readfile(mf)
            self.classification_res[model] = fcontent
        self.__compute_metrics()

    def __getfiles(self):
        os.chdir(self.infolder)
        for f in glob.glob("*.{0:s}".format(self.ext)):
            self.metric_files.append(f)
        print len(self.metric_files)

    @staticmethod
    def __readfile(f):
        with open(f, 'r') as _file:
            _file_content = _file.read()#.replace('\n', '')
            return _file_content

    def __compute_metrics(self):
        #pParams = re.compile("pattern")
        #pTime = re.compile("pattern")
        #pHighROC = re.compile("pattern")
        pF1 = re.compile("^F-measure = (.*)$", re.MULTILINE)
        #pGmean = re.compile("pattern")
        #pPhi = re.compile("pattern")
        #pBalance = re.compile("pattern")
        for model, content in self.classification_res.iteritems():
            for match in re.finditer(pF1, content):
                print model + match.group()

if __name__ == '__main__':
    # default CL arg values
    outfile = 'aggregate-metrics.csv'
    sep = ','
    ext = 'txt'

    try:
        if (len(sys.argv) <= 1):
            raise (getopt.GetoptError("No arguments!"))
        else:
            opts, args = getopt.getopt(sys.argv[1:], "hi:o:e:s:",
                                       ["help", "in=", "out=", "ext=", "sep="])
    except getopt.GetoptError:
        print('Wrong or no arguments. Please, enter\n\n'
              '\t%s [-h|--help]\n\n'
              'for usage info.' % __script__)
        sys.exit(2)

    for opt, arg in opts:
        if opt in ("-h", "--help"):
            print('Usage: {0:s} [OPTIONS]\n'
                  '\t-h, --help                                prints this help\n'
                  '\t-i, --in   <path/to/metrics/folder.txt>   path to metric files\n'
                  '\t-o, --out  <output.csv>                   name of the csv output file\n'
                  '\t-e, --ext  <txt>                          extension of metric files'
                  '\t-s, --sep  <,|;>                          either , or ; as separator'.format(__script__))
            sys.exit()
        elif opt in ("-i", "--in"):
            infolder = arg
        elif opt in ("-o", "--out"):
            outfile = arg
        elif opt in ("-e", "--ext"):
            ext = arg
        elif opt in ("-s", "--sep"):
            sep = arg

    cm = ComputeMetrics(infolder, outfile, ext, sep)
    cm.main()

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
    metrics = None
    permetric_vals = None
    classification_res = None

    # ['nb', 'knn', 'gpls', 'earth', 'glm', 'nnet', 'avNNet', 'pcaNNet', 'rbfDDA', 'mlp', 'mlpWeightDecay',
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
        self.metrics = dict()

    def main(self):
        self.__getfiles()
        for mf in self.metric_files:
            model = string.split(mf, sep=".")[0]
            fcontent = self.__readfile(mf)
            self.classification_res[model] = fcontent

        for model, content in self.classification_res.iteritems():
            self.permetric_vals = self.__compute_metrics(content)
            self.metrics[model] = self.permetric_vals
        pass

    def __getfiles(self):
        os.chdir(self.infolder)
        for f in glob.glob("*.{0:s}".format(self.ext)):
            self.metric_files.append(f)

    @staticmethod
    def __readfile(f):
        with open(f, 'r') as _file:
            _file_content = _file.read()  # .replace('\n', '')
            return _file_content

    @staticmethod
    def __compute_metrics(content):
        permetric_vals = dict()

        pParams = re.compile("The final values* used for the model (was|were) (.*\n*.*)\.")
        Params_vals = list()
        pTime = re.compile("Time difference of (.*) \w+")
        Time_vals = list()
        pHighROC = re.compile(".*TrainSpec\s+method\n1\s+(\d.\d+)")
        HighROC_vals = list()
        pF1 = re.compile("^F-measure = (.*)$", re.MULTILINE)
        F1_vals = list()
        pGmean = re.compile("^G-mean = (.*)$", re.MULTILINE)
        Gmean_vals = list()
        pPhi = re.compile("^Matthews phi = (.*)$", re.MULTILINE)
        Phi_vals = list()
        pBal = re.compile("^Balance = (.*)$", re.MULTILINE)
        Bal_vals = list()

        for match in re.finditer(pParams, content):
            if match is not None:
                Params_vals.append(match.group(2).replace('\n', ''))
        if len(Params_vals) is 0:
            pParams = re.compile("Tuning parameter \'(.*)\' was held constant at a value of (.*)")
            for match in re.finditer(pParams, content):
                assert(match is not None)
                Params_vals.append(match.group(1) + " = " + match.group(2))
        permetric_vals['parameters'] = Params_vals
        for match in re.finditer(pTime, content):
            assert (match is not None)
            Time_vals.append(match.group(1))
        permetric_vals['time'] = Time_vals
        for match in re.finditer(pHighROC, content):
            assert (match is not None)
            HighROC_vals.append(match.group(1))
        permetric_vals['ROC'] = HighROC_vals
        for match in re.finditer(pF1, content):
            assert (match is not None)
            F1_vals.append(match.group(1))
        permetric_vals['F1'] = F1_vals
        for match in re.finditer(pGmean, content):
            assert (match is not None)
            Gmean_vals.append(match.group(1))
        permetric_vals['G-mean'] = Gmean_vals
        for match in re.finditer(pPhi, content):
            assert (match is not None)
            Phi_vals.append(match.group(1))
        permetric_vals['Phi'] = Phi_vals
        for match in re.finditer(pBal, content):
            assert (match is not None)
            Bal_vals.append(match.group(1))
        permetric_vals['Balance'] = Bal_vals

        return permetric_vals


if __name__ == '__main__':
    # default CL arg values
    outfile = 'aggregate-metrics.csv'
    sep = ';'
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

#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jan 27 22:27:15 2019

@author: vaibhav
"""

from datetime import datetime
from dateutil.parser import parse
import pandas as pd
import re
import json

myStr ="2018/12/15 12:08:15.2987"
myStr = "12:00:01.10578"
myStr = "01:01.105786"
myStr = "0:01:52.551"

def stringToTime(myStr):
    
    handled = False
    yearFormatFlag = False
    
    match = re.search(r'(\d+/\d+/\d+ \d+:\d+:\d+.\d+)',myStr)
    if match != None:
        handled = True
        date_time_obj = datetime.strptime(myStr,'%Y/%m/%d %H:%M:%S.%f')
        yearFormatFlag =  True
        pass
    
    match = re.search(r'(\d+:\d+\d+.\d+)',myStr)
    if handled == False and match != None:
        handled = True
        date_time_obj = datetime.strptime(myStr,'%H:%M:%S.%f')
        pass
    
    match = re.search(r'(\d+:\d+:\d+)',myStr)
    if handled == False and match != None:
        handled = True
        date_time_obj = datetime.strptime(myStr,'%H:%M:%S')
        pass
    
    match = re.search(r'(\d+:\d+.\d+)',myStr)
    if handled == False and match != None:
        handled = True
        date_time_obj = datetime.strptime(myStr,'%M:%S.%f')
        pass
    
    match = re.search(r'(\d+.\d+)',myStr)
    if handled == False and match != None:
        handled = True
        date_time_obj = datetime.strptime(myStr,'%S.%f')
        pass
    
    if yearFormatFlag == True:
        FormBase_msec =date_time_obj - datetime(1970,1,1)
    else:
        FromBase_msec =date_time_obj - datetime(1900,1,1)
    FromBase_totalSec = FromBase_msec.total_seconds()
    return(FromBase_totalSec)

stringToTime(myStr)        
        
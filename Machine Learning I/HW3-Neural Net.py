#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Nov 22 17:21:18 2021

@author: huangzm
"""

import numpy as np
import pandas as pd
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Activation
from tensorflow.keras import optimizers

NEpochs = 10000
BatchSize=250
Optimizer=optimizers.RMSprop(learning_rate=0.001)

# Read in the data

TrainData = pd.read_csv('/Users/huangzm/Desktop/ML 1/HW3/NNHWTrain.csv',sep=',',header=0,quotechar='"')
ValData = pd.read_csv('/Users/huangzm/Desktop/ML 1/HW3/NNHWVal.csv',sep=',',header=0,quotechar='"')
TestData = pd.read_csv('/Users/huangzm/Desktop/ML 1/HW3/NNHWTest.csv',sep=',',header=0,quotechar='"')

# Rescale the training data

TrX = np.array(TrainData.iloc[:,0:57])
TrXrsc = (TrX - TrX.min(axis=0))/TrX.ptp(axis=0)
print(TrXrsc.shape)
print(TrXrsc.min(axis=0))
print(TrXrsc.max(axis=0))

# Rescale the validation data

ValX = np.array(ValData.iloc[:,0:57])
ValXrsc = (ValX - TrX.min(axis=0))/TrX.ptp(axis=0)
print(ValXrsc.shape)
print(ValXrsc.min(axis=0))
print(ValXrsc.max(axis=0))

# Rescale the test data

TeX = np.array(TestData.iloc[:,0:57])
TeXrsc = (TeX - TrX.min(axis=0))/TrX.ptp(axis=0)
print(TeXrsc.shape)
print(TeXrsc.min(axis=0))
print(TeXrsc.max(axis=0))


#%% Set up Neural Net Model

SpiralNN = Sequential()

SpiralNN.add(Dense(units=20,activation="relu",use_bias=True))
SpiralNN.add(Dense(units=1,activation="sigmoid",use_bias=True))

SpiralNN.compile(loss='binary_crossentropy', optimizer=Optimizer,metrics=['binary_crossentropy','accuracy'])
#print(SpiralNN.summary())

from tensorflow.keras.callbacks import EarlyStopping

StopRule = EarlyStopping(monitor='val_loss',mode='min',verbose=0,patience=100,min_delta=0.0)
FitHist = SpiralNN.fit(TrXrsc,TrainData[['IsSpam']],validation_data=(ValXrsc,ValData[['IsSpam']]), \
                    epochs=NEpochs,batch_size=BatchSize,verbose=0, \
                    callbacks=[StopRule])
    
#FitHist = SpiralNN.fit(TrXrsc,TrColorCode,epochs=NEpochs,batch_size=BatchSize,verbose=0)

print("Number of Epochs = "+str(len(FitHist.history['accuracy'])))
print("Final training accuracy: "+str(FitHist.history['accuracy'][-1]))
print("Recent history for training accuracy: "+str(FitHist.history['accuracy'][-10:-1]))
print("Final validation accuracy: "+str(FitHist.history['val_accuracy'][-1]))
print("Recent history for validation accuracy: "+str(FitHist.history['val_accuracy'][-10:-1]))


SpiralNN.summary()
SpiralNN.weights

#%% Make Predictions

TrPRed = SpiralNN.predict(TrXrsc,batch_size=TrXrsc.shape[0])
ValPRed = SpiralNN.predict(ValXrsc,batch_size=TrXrsc.shape[0])
TestPRed = SpiralNN.predict(TeXrsc,batch_size=TrXrsc.shape[0])

TrainData['TrPRed'] = TrPRed
ValData['ValPRed'] = ValPRed
TestData['TestPRed'] = TestPRed

TrainData.to_csv('/Users/huangzm/Desktop/ML 1/HW3/SpamNNWideTrainDFOutput.csv',
               sep=',',na_rep="NA",header=True,index=False)
ValData.to_csv('/Users/huangzm/Desktop/ML 1/HW3/SpamNNWideValDFOutput.csv',
               sep=',',na_rep="NA",header=True,index=False)
TestData.to_csv('/Users/huangzm/Desktop/ML 1/HW3/SpamNNWideTestDFOutput.csv',
               sep=',',na_rep="NA",header=True,index=False)

#%% Make Predictions

NNdeep = Sequential()

NNdeep.add(Dense(units=4,activation="relu",use_bias=True))
NNdeep.add(Dense(units=4,activation="relu",use_bias=True))
NNdeep.add(Dense(units=4,activation="relu",use_bias=True))
NNdeep.add(Dense(units=4,activation="relu",use_bias=True))
NNdeep.add(Dense(units=4,activation="relu",use_bias=True))
NNdeep.add(Dense(units=1,activation="sigmoid",use_bias=True))

NNdeep.compile(loss='binary_crossentropy', optimizer=Optimizer,metrics=['binary_crossentropy','accuracy'])

Fit2 = NNdeep.fit(TrXrsc,TrainData[['IsSpam']],validation_data=(ValXrsc,ValData[['IsSpam']]), \
                    epochs=NEpochs,batch_size=BatchSize,verbose=0, \
                    callbacks=[StopRule])
    
#FitHist = SpiralNN.fit(TrXrsc,TrColorCode,epochs=NEpochs,batch_size=BatchSize,verbose=0)

print("Number of Epochs = "+str(len(Fit2.history['accuracy'])))
print("Final training accuracy: "+str(Fit2.history['accuracy'][-1]))
print("Recent history for training accuracy: "+str(Fit2.history['accuracy'][-10:-1]))
print("Final validation accuracy: "+str(Fit2.history['val_accuracy'][-1]))
print("Recent history for validation accuracy: "+str(Fit2.history['val_accuracy'][-10:-1]))

NNdeep.summary()

#%% Make Predictions

TrPRed2 = NNdeep.predict(TrXrsc,batch_size=TrXrsc.shape[0])
ValPRed2 = NNdeep.predict(ValXrsc,batch_size=TrXrsc.shape[0])
TestPRed2 = NNdeep.predict(TeXrsc,batch_size=TrXrsc.shape[0])

TrainData['TrPRed2'] = TrPRed2
ValData['ValPRed2'] = ValPRed2
TestData['TestPRed2'] = TestPRed2

TrainData.to_csv('/Users/huangzm/Desktop/ML 1/HW3/SpamNNDeepTrainDFOutput.csv',
               sep=',',na_rep="NA",header=True,index=False)
ValData.to_csv('/Users/huangzm/Desktop/ML 1/HW3/SpamNNDeepValDFOutput.csv',
               sep=',',na_rep="NA",header=True,index=False)
TestData.to_csv('/Users/huangzm/Desktop/ML 1/HW3/SpamNNDeepTestDFOutput.csv',
               sep=',',na_rep="NA",header=True,index=False)

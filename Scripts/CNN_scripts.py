import tensorflow as tf
import keras
from keras.models import Sequential
from keras.layers import Conv2D
from keras.layers import MaxPooling2D
from keras.layers import Flatten
from keras.layers import Dense
from keras.layers import Dropout
from keras.layers import AveragePooling2D

classifier = Sequential()

classifier.add(Conv2D(1, (3,1), input_shape = (4364, 104, 1), activation = 'relu'))
classifier.add(MaxPooling2D(pool_size = (3, 1)))
classifier.add(Conv2D(1, (3, 1), activation = 'relu'))
classifier.add(MaxPooling2D(pool_size = (3, 1)))


classifier.add(Flatten())
#classifier.add(Dense(units = 100, activation = 'relu',kernel_initializer='he_uniform'))
classifier.add(Dense(units = 40, activation = 'relu',kernel_initializer='he_uniform'))
classifier.add(Dense(units = 12, activation = 'softmax'))

#classifier.compile(optimizer = "adam", loss = 'categorical_crossentropy', metrics = ['accuracy'])
Nadam =keras.optimizers.Adam(lr=0.1, beta_1=0.9, beta_2=0.999, epsilon=None, decay=0.0, amsgrad=True)

classifier.compile(optimizer = "adam", loss = 'categorical_crossentropy', metrics = ['accuracy'])

# Part 2 - Fitting the CNN to the images

from keras.preprocessing.image import ImageDataGenerator

train_datagen = ImageDataGenerator(rescale = 1./255)

test_datagen = ImageDataGenerator(rescale = 1./255)

training_set = train_datagen.flow_from_directory('~/Simulated_datasets/training_dataset',
                                                 target_size = (4364,104),
                                                 batch_size = 100,
                                                 class_mode = 'categorical',
                                                 color_mode='grayscale',
                                                 shuffle=True)

test_set = test_datagen.flow_from_directory('~/Simulated_datasets/test_dataset',
                                            target_size = (4364, 104),
                                            batch_size = 100,
                                            class_mode = 'categorical',
                                            color_mode='grayscale',
                                            shuffle=False)

classifier.fit(training_set,
                        steps_per_epoch = int(24000/100),
                         epochs = 3,
                         validation_data = test_set,
                         validation_steps = int(6000/100))

import pandas as pd
import seaborn as sn
import matplotlib.pyplot as plt
import numpy as np

import sklearn.metrics as metrics
from sklearn.metrics import confusion_matrix as cm

batch_size = 100

test_set2=test_set

test_set2.reset()
Y_pred = classifier.predict_generator(test_set2, steps = 6000 // batch_size)
y_pred = np.argmax(Y_pred, axis=-1)

pd.DataFrame(Y_pred).to_csv('~/CNN_calibration.csv')

matrix = cm(test_set2.classes, y_pred)
print(matrix)

#data = pd.DataFrame({'Bottleneck': matrix[:, 0], 'Constant': matrix[:, 1], 'Expansion': matrix[:, 2]})
#data.index = ['Bottleneck', 'Constant', 'Expansion']
#data.to_csv('/fs/project/PAA0202/Emanuel/Norops/Part_II/1_CNN_script/Confusion_matrix.csv')

import numpy as np
from keras.preprocessing import image
import tensorflow as tf
import os
import glob

print(test_set.class_indices)

folder_path = '~/Datasets'
os.chdir(folder_path)
files = glob.glob('*png')

for img in files:
	test_image = tf.keras.preprocessing.image.load_img(img, target_size = (4364, 104),color_mode='grayscale')
	test_image = image.img_to_array(test_image)
	test_image = np.expand_dims(test_image, axis = 0)
	test_image = np.vstack([test_image])
	result = classifier.predict(test_image)

	result2 = classifier.predict_proba(test_image)
	if result[0][0] == 1:
		prediction = 'Model1'
	elif result[0][1] == 1:
		prediction = 'Model2'
	elif result[0][2] == 1:
		prediction = 'Model3'
	elif result[0][3] == 1:
		prediction = 'Model4'
	elif result[0][4] == 1:
		prediction = 'Model5'
	elif result[0][5] == 1:
		prediction = 'Model6'
	elif result[0][6] == 1:
		prediction = 'Model7'
	elif result[0][7] == 1:
		prediction = 'Model8'
	elif result[0][8] == 1:
		prediction = 'Model9'
	elif result[0][9] == 1:
		prediction = 'Model10'
	elif result[0][10] == 1:
		prediction = 'Model11'
	elif result[0][11] == 1:
		prediction = 'Model12'

	print(prediction)
	print(result2)
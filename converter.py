#!/usr/bin/env python2.7

from PIL import Image, ImageDraw
from scipy import io
from scipy import misc
from scipy import ndimage
from scipy import ndarray
from time import gmtime, strftime
import argparse
import cv2
import h5py
import numpy
import os
import re
import sys
import xml.etree.ElementTree as et

###########################################################################
# abstract importer class
###########################################################################

class TrackDictionary(object):
	def __init__(self):
		self.dictionary={}
		self.tracks={}
		self.mothers={}
		self.daughters={}
		self.counter=0

	def add(self, objectID, frameID, object_grp):
		if (objectID,frameID) in self.dictionary:
			trackID=self.dictionary[(objectID, frameID)]
			if trackID in self.tracks.keys():
				self.tracks[trackID].append(object_grp)
			else:
				self.tracks[trackID]=[object_grp]
			return trackID
		else:
			self.dictionary[(objectID, frameID)]=self.counter
			self.tracks[self.counter]=[object_grp]
			self.counter+=1
			return self.counter-1

	def register(self, trackID, links):
		if len(links) == 1:
			self.dictionary[links[0]]=trackID
		elif len(links) > 1:
			daughterIDs=[]
			for i in links:
				self.dictionary[i]=self.counter
				self.mothers[self.counter]=trackID
				daughterIDs.append(self.counter)
				self.counter+=1
			self.daughters[trackID]=daughterIDs

	def getDaughter(self, trackID):
		if trackID in self.daughters:
			return self.daughters[trackID]
		else:
			return None
	def getDaughters(self):
		return self.daughters

	def getID(self, objectID, frameID):
		if (objectID, frameID) in self.dictionary:
			return self.dictionary[(objectID, frameID)]
		else:
			return None

	def getMother(self, trackID):
		if trackID in self.mothers:
			return self.mothers[trackID]
		else:
			return None

	def getMothers(self):
		return self.mothers

	def getTrack(self, trackID):
		if trackID in self.tracks:
			return self.tracks[trackID]
		else:
			return None

	def getTracks(self):
		return self.tracks

class PackedMask(object):
	def __init__(self, packed, length, bounding_box):
		self.packed = packed
		self.length = length
		self.bounding_box = bounding_box

	def getBoundingBox(self):
		return self.bounding_box

	def getLenght(self):
		return self.length

	def getPacked(self):
		return self.packed

	def getOutline(self):
		dimensions=[self.bounding_box[1,0]-self.bounding_box[0,0], self.bounding_box[1,1]-self.bounding_box[0,1]]
		image=numpy.unpackbits(self.packed)[0:self.length]
		image=numpy.array(numpy.reshape(image, tuple(dimensions)))
		data=numpy.pad(numpy.flipud(image.transpose()), 1, 'constant').copy()
		contours = cv2.findContours(data, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE, offset=(self.bounding_box[0,0]-1,self.bounding_box[0,1]))[1]
		if len(contours)>0:
			contour=numpy.asarray(contours[0], dtype='uint32')
			return numpy.reshape(contour, contour.shape[::2])
		else:
			return []



class Object(object):

	def __init__(self, object_id, centroid, packedMask, outline, links):
		self.object_id = object_id
		self.centroid = centroid
		self.packedMask = packedMask
		self.outline = outline
		self.links = links

	def getObjectID(self):
		return self.object_id

	def getCentroid(self):
		return self.centroid

	def getBoundingBox(self):
		return self.packedMask.getBoundingBox()

	def getPackedMask(self):
		return self.packedMask

	def getOutline(self):
		return self.outline

	def getLinks(self):
		return self.links

class Importer(object):

	def __init__(self):
		self.object_groups={}
		self.track_dictionary=TrackDictionary()
		self.trackAnnotations={}
		self.objectAnnotations={}
		self.annotations={}
		self.events={}
		self.multiMoves={}
		self.images=[]

	def getAnnotations(self):
		'''
		Is called before annotations are writen to the hdf5 file, should fill up self.trackAnnotations and self.objectAnnotations dictionaries.
		For self.trackAnnotations the dictionary has the following form
			key: the trackID 
			value: a string containing the description of the annotation
		For self.objectAnnotations the dictionary has the following form !!!writing annotations to objects is not yet implemented!!!
			key: a tuple (objectID, time) referencing a specific object
			value: a string containing the description of the annotation
		'''
		raise NotImplementedError()

	def getImageDimensions(self):
		'''
		Returns the image dimensions of the images evaluated during tracking.
		'''
		raise NotImplementedError()

	def getNumberOfFrames(self):
		'''
		Returns the number of the images evaluated during tracking.
		'''
		raise NotImplementedError()

	def getObjects(self, frameID):
		'''
		Returns the objects for the given frame.
		Where the returned dictionary contains the following entries:
			key: integer object id
			value: a tuple containing the following information in the given order (centroid, boundingBox, packedMask, size, links)

				centroid 		- 	a 1-dimensional numpy array with the center coordinates of the object [x,y,...]
				boundingBox 	- 	a 2-dimensional numpy array with the bounding box coordinates of the object [[x1,y1,...], [x2,y2,...]]
				packedMask 		- 	an 1-dimensional array of the flattened n-dimensional mask, packed with numpy.packbits
				size			- 	the number of entries of the unpacked, flattened n-dimensions mask
				links 			-	a list of object ids [id1,...] specifying the associated object(s) in the future frame !!!should be tuples (frameID, objectID) to support jumbs over several frames!!!

			It is also allowed to fill up the self.objectAnnotations dictionary etc.
		'''
		raise NotImplementedError()

	def getTracks(self):
		'''
		This function is called before tracks are written to the h5 format.
		If the getObjects() function can't supply the links between objects this function fills up self.tracks and self.trackAnnotations, such that 
		tracks get written to the h5 file properly.

		self.tracks is a dictionary containing the follwoing information:
			key: an integer track id
			value: a list of hdf5 groups of the objects corresponding to the track

		The object groups can be accessed through self.object_groups by self.object_groups[(objectID,time)]. self.object_groups is filled when 
		objects are read from the input files.
		'''
		raise NotImplementedError()

	def writeInfo(self, info_grp):
		'''
		This function es called at the end of the conversion process to add additional information for the specific data format, which is converted.
		The information can be directly written to the supplied info_grp and is saved under the group name 'meta_info'.
		'''
		raise NotImplementedError()

	def setImageFolder(self, folder):
		'''
		Sets the image folder and appends image files to tracking file.
		'''
		if os.path.exists(folder):
			self.images=filter(lambda file: any(file.lower().endswith(x) for x in ['.png', '.tif', '.tiff', '.jpg', '.jpeg']),sorted(os.listdir(folder)))
			self.images=map(lambda file: os.path.join(folder,file), self.images)

	def writeAnnotations(self, file):
		#create annotations
		self.getAnnotations()
		#write annotations
		annotations_grp=file.create_group('/annotations')
		annotations_ref={}
		index=0
		annotations_grp.create_group('object_annotations')
		track_annotation_grp=annotations_grp.create_group('track_annotations')
		for key in self.annotations:
			annot_grp=track_annotation_grp.create_group(str(index))
			annotations_ref[key]=annot_grp
			annot_grp.create_dataset('id', data=key, dtype='uint16')
			annot_grp['description']=numpy.string_(self.annotations[key])
			index+=1

	def writeEvents(self, file):
		self.events["cell_death"]=(0, "The cell is identified as dead.")
		self.events["cell_division"]=(1, "The cell divides in 2 or more other cells.")
		self.events["cell_lost"]=(2, "The cell is lost during the tracking process.")
		self.events["cell_merge"]=(3, "Two or more cells are merged to on object.")
		self.events["cell_unmerge"]=(4, "Two or more cells are merged to on object.")
		self.events["end_of_movie"]=(5, "The cell track ends at the end of the movie.")

		events_grp=file.create_group('events')
		for key in self.events:
			event_grp=events_grp.create_group(str(key))
			event_grp.create_dataset('event_id', data=self.events[key][0], dtype='uint16')
			event_grp['name']=key
			event_grp['description']=self.events[key][1]


	def writeImages(self, file):
		if len(self.images) > 0:
			images_grp=file.create_group('images')
			images_grp.create_dataset('frame_rate', data=-1., dtype='float32')
			images_grp.create_dataset('nframes', data=len(self.images), dtype='uint32')
			images_grp.create_dataset('nslices', data=1, dtype='uint16')
			images_grp.create_dataset('slice_shape', data=(1,1), dtype='uint16')
			frames_grp=images_grp.create_group('frames')
			index=0
			for image in self.images:
				sys.stdout.write('\r/images/frames/' + str(index)) 
				sys.stdout.flush()
				frame_grp=frames_grp.create_group(str(index))
				frame_grp.create_dataset('frame_id', data=index, dtype='uint32')
				slice_grp=frame_grp.create_group('slices')
				slice_grp=slice_grp.create_group('0')
				image_data=misc.imread(image)
				slice_grp.create_dataset('dimensions', data=image_data.shape, dtype='uint32')
				slice_grp.create_dataset('nchannels', data=1, dtype='uint16')
				slice_grp.create_dataset('slice_id', data=0, dtype='uint16')
				channel_grp=slice_grp.create_group('channels')
				channel_grp.create_dataset('0', data=image_data, chunks=image_data.shape, compression='gzip')
				index+=1
			sys.stdout.write('\r                                         ') 
			sys.stdout.flush()

	def writeObjects(self, file):
		numberOfFrames=self.getNumberOfFrames()
		objects_grp=file.create_group('objects')

		if 'images' in file.keys():
			objects_grp['frame_rate'] = h5py.SoftLink('/images/frame_rate')
			objects_grp['nframes'] = h5py.SoftLink('/images/nframes')
			objects_grp['nslices'] = h5py.SoftLink('/images/nslices')
			objects_grp['slice_shape'] = h5py.SoftLink('/images/slice_shape')
		else:
			objects_grp.create_dataset('frame_rate', data=-1., dtype='float32')
			objects_grp.create_dataset('nframes', data=numberOfFrames, dtype='uint32')
			objects_grp.create_dataset('nslices', data=1, dtype='uint16')
			objects_grp.create_dataset('slice_shape', data=(1,1), dtype='uint16')

		objects_grp=objects_grp.create_group('frames')
		for frameID in range(numberOfFrames):
			sys.stdout.write('\r/objects/frames/' + str(frameID))
			sys.stdout.flush()

			#create frame group
			frame_grp=objects_grp.create_group(str(frameID))
			frame_grp.create_dataset('frame_id', data=frameID, dtype='uint32')

			#create slice group
			slice_grp=frame_grp.create_group('slices')
			slice_grp=slice_grp.create_group('0')
			slice_grp.create_dataset('slice_id', data=0, dtype='uint16')

			if 'images' in file.keys():
				slice_grp['dimensions'] = h5py.SoftLink('/images/frames/' + str(frameID) + '/slices/0/dimensions')
				slice_grp['nchannels'] = h5py.SoftLink('/images/frames/' + str(frameID) + '/slices/0/nchannels')
			else:
				slice_grp.create_dataset('nchannels', data=1, dtype='uint16')

			#create channel group
			channels_grp=slice_grp.create_group('channels')
			channel_grp=channels_grp.create_group('0')
			channel_grp.create_dataset('channel_id', data=0, dtype='uint16')
			sub_objects_grp=channel_grp.create_group('objects')
			
			objects=self.getObjects(frameID)
			for objectID in objects:
				#take object
				obj=objects[objectID]

				#create object
				object_grp=sub_objects_grp.create_group(str(objectID))
				object_grp.create_dataset('frame_id', data=frameID, dtype='uint32')
				object_grp.create_dataset('object_id', data=objectID, dtype='uint32')
				object_grp.create_dataset('slice_id', data=0, dtype='uint32')
				object_grp.create_dataset('channel_id', data=0, dtype='uint16')
				self.object_groups[(objectID,frameID)]=object_grp

				#write centroid
				dataset=object_grp.create_dataset('centroid',data=obj.getCentroid(), dtype=obj.getCentroid().dtype)

				#write bounding box
				dataset=object_grp.create_dataset('bounding_box',data=obj.getBoundingBox(), dtype=obj.getBoundingBox().dtype)

				#write packed mask
				dataset=object_grp.create_dataset('packed_mask', data=obj.getPackedMask().getPacked(), dtype=obj.getPackedMask().getPacked().dtype)
				dataset.attrs.create('length', obj.getPackedMask().getLenght(), dtype='uint32')

				#write outline
				dataset=object_grp.create_dataset('outline', data=obj.getOutline())

				if (objectID, frameID) in self.objectAnnotations:
					annot_grp=object_grp.create_group('annotations')
					index=0
					for annot in self.objectAnnotations[(objectID, frameID)]:
						annot_grp[str(index)]=annotations_ref[annot]

				trackID=self.track_dictionary.add(objectID, frameID, object_grp)

				links=obj.getLinks()
				self.track_dictionary.register(trackID, links)

				# if (objectID, frameID) in self.multiMoves:
				# 	track_dict[self.multiMoves[(objectID, frameID)]]=trackID

			sys.stdout.write('\r                                         ') 
			sys.stdout.flush()

	def writeTracks(self, file):

		#create tracks for other formats
		self.getTracks()
		
		#write tracks to hdf5 file
		tracks_grp=file.create_group('/autotracklets')
		track_references={}
		numberOfFrames=self.getNumberOfFrames()
		daughter_ids=numpy.sort(numpy.array(self.track_dictionary.getDaughters()).flatten())

		trackCounter=0
		for trackID in self.track_dictionary.getTracks():
			if len(self.track_dictionary.getTrack(trackID))>1 or (trackID in daughter_ids):
				track=tracks_grp.create_group(str(trackCounter))
				track_references[trackID]=track
				trackCounter+=1

		trackCounter=0
		for trackID in self.track_dictionary.getTracks():
			if len(self.track_dictionary.getTrack(trackID))>1 or (trackID in daughter_ids):
				sys.stdout.write('\r/autotracklets/' + str(trackCounter)) 
				sys.stdout.flush()

				#creating hdf5 track group
				track=tracks_grp[str(trackCounter)]
				track.create_dataset('autotracklet_id', data=trackCounter, dtype='uint32')

				if trackID in self.trackAnnotations:
					annot_grp=track.create_group('annotations')
					index=0
					for annot in self.trackAnnotations[trackID]:
						annot_grp[str(index)]=annotations_ref[annot]
				trackCounter+=1
	
				#add hdf5 objects to track
				trackObjects=track.create_group('objects')
				index=0
				for i in self.track_dictionary.getTrack(trackID):
					trackObjects[str(index)]=h5py.SoftLink(i.name)
					index+=1
	
				#write length of track
				ids=map(lambda object : object['frame_id'][()], self.track_dictionary.getTrack(trackID))
				[start,end]=[min(ids),max(ids)]
				track.create_dataset('start', data=start, dtype='uint32')
				track.create_dataset('end', data=end, dtype='uint32')
				if end==(numberOfFrames-1):
					track['next_event']=h5py.SoftLink('/events/end_of_movie')
			sys.stdout.write('\r                                         ') 
			sys.stdout.flush()

		for key in self.track_dictionary.getMothers():
			if key in track_references and self.track_dictionary.getMother(key) in track_references:
				track_grp=track_references[key]
				track_grp['previous_event']=h5py.SoftLink('/events/cell_division')
				previous=track_grp.create_group('previous')
				previous[str(0)]=h5py.SoftLink(track_references[self.track_dictionary.getMother(key)].name)
				#track_references[key].create_dataset('mother', data=self.track_dictionary.getMother(key), dtype='uint32')
					
		for key in self.track_dictionary.getDaughters():
			if key in track_references:
				#if 'event' not in track_references[key].keys(): #this error got to be fixed!!
					#track_references[key].create_dataset('next_event', data=self.events["cell_division"][0], dtype='uint16')
				track_grp=track_references[key]
				track_grp['next_event']=h5py.SoftLink('/events/cell_division')
				next=track_grp.create_group('next')
				index=0
				for daughter in self.track_dictionary.getDaughter(key):
					if daughter in track_references:
						next[str(index)]=h5py.SoftLink(track_references[daughter].name)
						index+=1
					else:
						print "ERROR: daughter track '" + str(daughter) + "' not found for track '" + str(key) + "'!"

				#track_references[key].create_dataset('daughters', data=self.track_dictionary.getDaughters(key), dtype='uint32')
				
	def exportToHDF5(self, fileName):
		'''
		Exports the data to the given hdf5 file 'fileName'.
		'''

		#open file
		file=h5py.File(fileName,'w', libver='latest')
		file['data_format_version']=numpy.string_('1.0')
		file['coordinate_format']=numpy.string_('Cartesian')

		#write annotations	
		self.writeAnnotations(file)

		#write events
		self.writeEvents(file)

		#write images	
		self.writeImages(file)

		#write objects
		self.writeObjects(file)

		#write tracks
		self.writeTracks(file)
	
		#write info
		info_grp=file.create_group('info')
		self.writeInfo(info_grp)
			

		file.close()
		sys.stdout.write('\rfinished sucessfully' + '      ')
		sys.stdout.flush()
		print ''

###########################################################################
# importer class for the new xml tracking format (FluidTracking)
###########################################################################

class XMLImportFluidTracking(Importer):
	def __init__(self, fileName):
		super(XMLImportFluidTracking, self).__init__()
		if(not os.path.exists(fileName)):
			sys.exit("ERROR: File '" + fileName + "' doesn't exist!")
		self.fileName=fileName
		self.tree=et.parse(fileName)
		self.numberFrames=self.getNumberOfFrames()
		self.x,self.y=self.getImageDimensions()

	def getBoundingBox(self,object):
		box=[]
		for p in object.find('mask').find('bbox').iter('point'):
			box.append([int(p.find('x').text), self.y-int(p.find('y').text)])
		return numpy.asarray([[box[0][0],box[1][1]],[box[1][0],box[0][1]]],dtype='uint16')-[[0,1],[-1,0]]

	def getCentroid(self, object):
		p=object.find('centroid').find('point')
		box=[int(p.find('x').text), self.y-int(p.find('y').text)]
		return numpy.asarray(box,dtype='uint16')

	@staticmethod
	def decodeRLEString(rleString):
		decoded=[]
		for token in re.findall('[0-9]*[BW]',rleString):
			if token[-1] == 'B':
				decoded.extend(numpy.zeros(int(token.strip('B')),dtype='int8').tolist())
			elif token[-1] == 'W':
				decoded.extend(numpy.ones(int(token.strip('W')),dtype='int8').tolist())
		return numpy.asarray(decoded,dtype='uint8')

	@staticmethod
	def getLinks(object, frameID):
		links=[]
		for successor in object.find('links').findall('successor'):
			links.append((int(successor.find('objectID').text), frameID))
		return links

	def getAnnotations(self):
		return

	def getImageDimensions(self):
		dimensions=self.tree.getroot().find('description').find('imageDimensions')
		return (int(dimensions.find('width').text), int(dimensions.find('height').text))

	def getNumberOfFrames(self):
		return int(self.tree.getroot().find('frameList').find('numberOfFrames').text)

	def getFrame(self, id):
		if id < self.numberFrames:
			return self.tree.getroot().find('frameList').findall('frame')[id]
		else:
			return None

	def getObject(self, xmlObject, frameID):
		objectID=int(xmlObject.find('id').text)
		centroid=self.getCentroid(xmlObject)
		bbox=self.getBoundingBox(xmlObject)
		unpacked=XMLImportFluidTracking.decodeRLEString(xmlObject.find('mask').find('runLengthString').text)
		unpacked=numpy.transpose(numpy.reshape(unpacked,(bbox[1,1]-bbox[0,1],bbox[1,0]-bbox[0,0]))).flatten()
		packed=numpy.packbits(unpacked)
		packedMask=PackedMask(packed, unpacked.size, numpy.asarray(bbox, dtype='uint16'))
		links=XMLImportFluidTracking.getLinks(xmlObject, frameID+1)
		return Object(objectID, numpy.asarray([centroid], dtype='uint16'), packedMask, packedMask.getOutline(), links)

	def getObjects(self, frameID):
		frame=self.getFrame(frameID)
		objects={}
		for obj in frame.iter('object'):
			tmp=self.getObject(obj, frameID)
			objects[tmp.getObjectID()]=tmp
		return objects

	def getTracks(self):
		return

	def writeInfo(self, info_grp):
		info_grp['timeOfConversion']=numpy.string_(strftime("%d-%m-%Y-%H:%M:%S", gmtime()))
		info_grp['inputFiles']=[self.fileName]
		tps=info_grp.create_group('tracking_info')
		for i in self.tree.getroot().find('description').find('trackingInfo').findall('info'):
			value=i.find('value').text
		 	try:
		 		float(value)
		 	except ValueError:
		 		value=numpy.string_(value)
		 	except TypeError:
		 		value=numpy.string_(value)
		 	else:
		 		value=numpy.float32(value)
			tps[i.find('name').text]=value
		tps['timeOfTracking']=numpy.string_(self.tree.getroot().find('description').find('date').text)
		return

###########################################################################
# importer class for the old tracking format (FluidTracking)
###########################################################################

class XMLImportFluidTrackingOld(Importer):
	def __init__(self, label_folder, divisions_file, tracking_log):
		super(XMLImportFluidTrackingOld, self).__init__()
		self.label_files=filter(lambda file: any(file.lower().endswith(x) for x in ['.mat']),sorted(os.listdir(label_folder)))
		self.label_files=map(lambda file: os.path.join(label_folder,file), self.label_files)
		print 'Found ' + str(len(self.label_files)) + ' label files...'
		self.tracking_log=tracking_log
		self.divisions={}
		self.label_dict={}
		self.label_counter=0
		self.parseDivisionsFile(divisions_file)

	def parseDivisionsFile(self, file_name):
		file=open(file_name,'r')
		rulePattern='[0-9]+[\s]*->[\s]*[0-9]+'
		rules=re.findall(rulePattern,file.read())
		pairs=map(lambda x: numpy.asarray(re.findall('[0-9]+',x),dtype=int),rules)
		labels=numpy.asarray(pairs).flatten()
		for i in labels:
			if i not in self.label_dict:
				self.label_dict[i]=self.label_counter
				self.label_counter+=1
		map(lambda x: self.divisions.update({x[0]:self.divisions[x[0]]+[x[1]]}) if x[0] in self.divisions else self.divisions.update({x[0]:[x[1]]}) ,pairs)
		return

	def getAnnotations(self):
		return

	def getImageDimensions(self):
		return io.loadmat(self.label_files[0])['Expression1'].shape[::-1]

	def getNumberOfFrames(self):
		return len(self.label_files)

	def getObjects(self, frameID):
		frame=numpy.transpose(numpy.asarray(io.loadmat(self.label_files[frameID])['Expression1'],dtype=numpy.int))
		labels=numpy.unique(frame)[1:]

		objects={}
		for i in labels:
			if i not in self.label_dict:
				self.label_dict[i]=self.label_counter
				self.label_counter+=1
		
		ydim=frame.shape[1]
		centroids=map(lambda x: (x[0],ydim-x[1]), ndimage.measurements.center_of_mass(frame,frame, labels))
		
		
		next=[]
		if frameID+1 < self.getNumberOfFrames():
			next=numpy.unique(io.loadmat(self.label_files[frameID+1])['Expression1'])[1:]

		for i in range(len(labels)):
			label=labels[i]
			sliced=ndimage.measurements.find_objects(frame==label)[0]
			roi=frame[sliced]
			unpacked=numpy.asarray(roi.flatten()==label,dtype='uint8')
			packed=numpy.packbits(unpacked)
			bbox=map(lambda x: [x.start, x.stop], sliced)
			bbox=numpy.transpose([bbox[0], bbox[1]])
			#bbox=map(lambda x: [x[0], ydim-x[1]], [bbox[0], bbox[1]])
			bbox=[[bbox[0,0],ydim-bbox[1,1]],[bbox[1,0],ydim-bbox[0,1]]]
			#get links
			if label in next:
				links=[(self.label_dict[label], frameID+1)]
			elif label in self.divisions:
				links=map(lambda x: (self.label_dict[x], frameID+1), self.divisions[label])
			else:
				links=[]
			packedMask=PackedMask(packed, unpacked.size, numpy.asarray(bbox, dtype='uint16'))
			obj=Object(self.label_dict[label], numpy.asarray([centroids[i]], dtype='uint16'), packedMask, packedMask.getOutline(), links)
			objects[obj.getObjectID()]=obj
		return objects

	def getTracks(self):
		return

	def writeInfo(self, info_grp):
		return


###########################################################################
# importer class for importing tracking results from ilastik
###########################################################################

class HDF5ImportIlastik(Importer):

	def __init__(self, objectLayerFile, trackingProjectFile):
		super(HDF5ImportIlastik, self).__init__()
		if(not os.path.exists(objectLayerFile)):
			sys.exit("ERROR: File '" + objectLayerFile + "' doesn't exist!")
		if(not os.path.exists(trackingProjectFile)):
			sys.exit("ERROR: File '" + trackingProjectFile + "' doesn't exist!")
		self.objectLayerFile=objectLayerFile
		self.trackingProjectFile=trackingProjectFile
		self.objectsFile=h5py.File(objectLayerFile,'r')
		self.frames=self.objectsFile[self.objectsFile.keys()[0]]
		self.tracksFile=h5py.File(trackingProjectFile,'r')
		self.readMultiMoves()

	def __del__(self):
		self.objectsFile.close()
		self.tracksFile.close()

	def getAnnotations(self):
		# if '/ConservationTracking' in self.tracksFile:
		# 	self.annotations[0]="Merged, object consists of several individual objects."
		# 	self.annotations[1]="Merged, track contains merged objects."
		return

	def getImageDimensions(self):
		return self.frames.shape[1:3]

	def getNumberOfFrames(self):
		return self.frames.shape[0]

	def getObjects(self, frameID):
		frame=self.frames[frameID]
		if frame.shape[-1]>1:
			frame=frame[:,:,:,1].reshape(frame.shape[0], frame.shape[1])
		else:
			frame=frame.reshape(frame.shape[0], frame.shape[1])
		# labeled, number_labels=ndimage.label(frame)
		image_slices = ndimage.measurements.find_objects(frame)
		labels=numpy.unique(frame)[1:]
		yDim=self.getImageDimensions()[1]
		centroids=map(lambda x: [x[0], yDim-x[1]],ndimage.measurements.center_of_mass(frame,frame,labels))
		objects={}
		next={}
		if '/ChaingraphTracking' in self.tracksFile:
			root='/ChaingraphTracking/EventsVector/0/'
		elif '/ConservationTracking' in self.tracksFile:
			root='/ConservationTracking/EventsVector/0/'
		if str(frameID+1) in self.tracksFile[root].keys():
			framePath=root+str(frameID+1)
			keys=self.tracksFile[framePath].keys()
			if 'mov' in keys:
				if 'multiMove' in keys:
					movements=numpy.asarray(self.tracksFile[framePath + '/mov'][()], dtype='uint32')
					multiMove=numpy.asarray(self.tracksFile[framePath + '/multiMove'][()], dtype='uint32')
					movements=movements[numpy.where(map(lambda x: x  not in multiMove[:,1], movements[:,1]))]
				else:
					movements=numpy.asarray(self.tracksFile[framePath + '/mov'][()], dtype='uint32')
				map(lambda x: next.update({x[0]:[(x[1], frameID+1)]}), movements)
			if 'div' in keys:
				divisions=numpy.asarray(self.tracksFile[framePath + '/div'][()], dtype='uint32')
				map(lambda x: next.update({x[0]:[(x[1], frameID+1), (x[2], frameID+1)]}), divisions)
			if 'merger' in keys:
				mergers=numpy.asarray(self.tracksFile[framePath + '/merger'][()], dtype='uint32')
				#map(lambda x: self.objectAnnotations.update({(x[0], frameID+1): [0]}), mergers)
		for i in range(len(labels)):
			roi=frame[image_slices[i]]
			unpacked=numpy.asarray(roi.flatten()==labels[i],dtype='uint8')
			packed=numpy.packbits(unpacked)
			bbox=map(lambda x: [x.start, x.stop], image_slices[i])
			bbox=numpy.transpose([bbox[0], bbox[1]])
			bbox=[[bbox[0,0], yDim-bbox[1,1]],[bbox[1,0], yDim-bbox[0,1]]]
			if labels[i] in next: 
				links=next[labels[i]]
			else:
				links=[]
			packedMask=PackedMask(packed, unpacked.size, numpy.asarray(bbox, dtype='uint16'))
			obj=Object(labels[i], numpy.asarray([centroids[i]], dtype='uint16'), packedMask, packedMask.getOutline(), links)
			objects[obj.getObjectID()]=obj
		return objects

	def getTracks(self):
		return

	def readMultiMoves(self):
		if '/ConservationTracking' in self.tracksFile:
			root='/ConservationTracking/EventsVector/0/'
			for frameID in range(self.getNumberOfFrames()):
				framePath=root+str(frameID)
				keys=self.tracksFile[framePath].keys()
				if 'multiMove' in keys:
					multiMove=numpy.asarray(self.tracksFile[framePath + '/multiMove'][()], dtype='uint32')
					map(lambda x: self.multiMoves.update({(x[0],x[2]):(x[1],frameID)}), multiMove)
		return

	def writeInfo(self, info_grp):
		info_grp['timeOfConversion']=numpy.string_(strftime("%d-%m-%Y-%H:%M:%S", gmtime()))
		info_grp['inputFiles']=[self.objectLayerFile, self.trackingProjectFile]
		tps=info_grp.create_group('tracking_info')
		tps['ilastik_version']=numpy.string_(self.tracksFile['ilastikVersion'][()])
		tps['timeOfTracking']=numpy.string_(self.tracksFile['time'][()])
		if '/ChaingraphTracking' in self.tracksFile:
			alg=numpy.string_('ChaingraphTracking')
		else:
			alg='ConservationTracking'
		tps['algorithm']=alg
		return

###########################################################################
# importer class for importing exported results from the Celltracker
###########################################################################

class XMLImportCelltracker(Importer):

	def __init__(self, metaFolder, tracksFile, someImage):
		super(XMLImportCelltracker, self).__init__()
		if(not os.path.exists(metaFolder)):
			sys.exit("ERROR: File '" + metaFolder + "' doesn't exist!")
		if(not os.path.exists(tracksFile)):
			sys.exit("ERROR: File '" + tracksFile + "' doesn't exist!")
		if(not os.path.exists(someImage)):
			sys.exit("ERROR: File '" + someImage + "' doesn't exist!")
		self.metaFolder=metaFolder
		self.tracksFile=tracksFile
		self.someImage=someImage
		self.metaFiles=filter(lambda file: any(file.lower().endswith(x) for x in ['.xml', '.XML']),sorted(os.listdir(metaFolder)))
		self.metaFiles=map(lambda file: os.path.join(metaFolder,file), self.metaFiles)
		print 'Found ' + str(len(self.metaFiles)) + ' meta files...'
		self.tracksXML=et.parse(tracksFile).getroot()
		self.someImage=someImage
		self.link_dictionary=self.readLinks()

	def getAnnotations(self):
		return

	def getImageDimensions(self):
		return Image.open(self.someImage).size

	def getNumberOfFrames(self):
		return len(self.metaFiles)

	def getObjects(self, frameID):
		frame=et.parse(self.metaFiles[frameID])
		objects={}
		yDim=self.getImageDimensions()[1]
		for obj in frame.getroot().iter('Object'):
			objectID=int(obj.find('ObjectID').find('value').text)
			tmp=obj.find('ObjectCenter').find('point')
			centroid=[float(tmp.find('x').text), yDim-float(tmp.find('y').text)]
			tmp=obj.find('ObjectBoundingBox')
			bbox=[]
			for point in tmp.iter('point'):
				bbox.append([int(point.find('x').text), int(point.find('y').text)])
			bbox=numpy.asarray(bbox, dtype='uint16').T
			tmp=obj.find('Outline')
			outline=[]
			for point in tmp.iter('point'):
				outline.append((float(point.find('x').text), float(point.find('y').text)))
			img = Image.new('L', self.getImageDimensions(), 0)
			ImageDraw.Draw(img).polygon(outline, outline=1, fill=1)
			mask = numpy.array(img)
			sliced_mask=mask[(slice(bbox[1][0],bbox[1][1],None), slice(bbox[0][0],bbox[0][1],None))]
			unpacked=sliced_mask.flatten()
			bbox=numpy.transpose(bbox)
			bbox=numpy.asarray([bbox[0], bbox[1]])
			unpacked=numpy.transpose(numpy.reshape(unpacked,(bbox[1,1]-bbox[0,1],bbox[1,0]-bbox[0,0]))).flatten()
			packed=numpy.packbits(unpacked)
			bbox=[[bbox[0,0], yDim-bbox[1,1]],[bbox[1,0], yDim-bbox[0,1]]]
			if (objectID,frameID) in self.link_dictionary:
				links=self.link_dictionary[(objectID,frameID)]
			else:
				links=[]
			packedMask=PackedMask(packed, unpacked.size, numpy.asarray(bbox, dtype='uint16'))
			obj=Object(objectID, numpy.asarray([centroid], dtype='uint16'), packedMask, packedMask.getOutline(), links)
			objects[obj.getObjectID()]=obj
		return objects

	def readLinks(self):
		link_dictionary={}
		for track in self.tracksXML.iter('Track'):
			trackID=int(track.find('TrackID').text)
			tmp=[]
			time=-1
			for obj in track.iter('object'):
				objectID=int(obj.find('ObjectID').text)
				time=int(obj.find('Time').text)-1
				if objectID>=0:
					tmp.append((objectID,time))
					if len(tmp) > 1:
						link_dictionary[tmp[-2]]=[(objectID,time)]
		# daughters=track.find('Daugters')
		# if daughters != None:
		# 	last=tmp[-1]
		# 	tmp=[]
		# 	for i in daughters.iter('DaugterID'):
		# 		tmp.append((int(i.text),time))
		# 	link_dictionary[last]=tmp
		return link_dictionary
			

	def getTracks(self):
		return

	def writeInfo(self, info_grp):
		info_grp['timeOfConversion']=numpy.string_(strftime("%d-%m-%Y-%H:%M:%S", gmtime()))
		info_grp['inputFiles']=[self.metaFolder, self.tracksFile, self.someImage]
		return


###########################################################################
# Parser specifications
###########################################################################

def query_yes_no(question, default="yes"):
    valid = {"yes": True, "y": True, "no": False, "n": False}
    if default is None:
        prompt = " [y/n] "
    elif default == "yes":
        prompt = " [Y/n] "
    elif default == "no":
        prompt = " [y/N] "
    else:
        raise ValueError("invalid default answer: '%s'" % default)

    while True:
        sys.stdout.write(question + prompt)
        choice = raw_input().lower()
        if default is not None and choice == '':
            return valid[default]
        elif choice in valid:
            return valid[choice]
        else:
            sys.stdout.write("Please respond with 'yes' or 'no' (or 'y' or 'n').\n")

parser=argparse.ArgumentParser(description='Conversion script for various tracking formats into h5 standard format.', formatter_class=argparse.RawTextHelpFormatter)
parser.add_argument('-o', metavar='output_file_name',type=str, help='name of the output file')

subparsers=parser.add_subparsers(dest='type', help='sub-command help')

#add subparser for FluidTracking xml format
parser_1 = subparsers.add_parser('1', help='FluidTracking xml format.', epilog='example usage: ./converter.py 1 2014-07-16_13:01:29-data-frame.xml image_folder')
parser_1.add_argument('xml_file', type=str)
parser_1.add_argument('image_folder', type=str, default="", nargs='?')

#add subparser for FluidTracking xml format
parser_2 = subparsers.add_parser('2', help='FluidTracking old data format.', epilog='example usage: ./converter.py 2 label_folder divisions.m image_folder tracking.log')
parser_2.add_argument('label_folder', type=str)
parser_2.add_argument('divisions_file', type=str)
parser_2.add_argument('image_folder', type=str, default="", nargs='?')
parser_2.add_argument('tracking_log', type=str, default="", nargs='?')

#add subparser for Ilastik tracking format
parser_3 = subparsers.add_parser('3', help='Ilastik tracking format.', epilog='example usage: ./converter.py 3 objects.h5 tracking.ilp image_folder')
parser_3.add_argument('object_layer_file', type=str)
parser_3.add_argument('tracking_project_file', type=str)
parser_3.add_argument('image_folder', type=str, default="", nargs='?')

#add subparser for Celltracker xml format
parser_4 = subparsers.add_parser('4', help='Celltracker xml format.', epilog='example usage: ./converter.py 4 meta_xml_folder tracksXML-export.xml someImage.png image_folder')
parser_4.add_argument('xml_folder', type=str)
parser_4.add_argument('tracks_xml', type=str)
parser_4.add_argument('some_image', type=str)
parser_4.add_argument('image_folder', type=str, default="", nargs='?')

args=parser.parse_args()

OUTPUT_FILE_NAME='tracking-data.h5'

if args.o != None:
	OUTPUT_FILE_NAME=args.o
	if os.path.exists(OUTPUT_FILE_NAME):
		if(query_yes_no("File '" + OUTPUT_FILE_NAME + "' already exists, override file?", default="no") == False):
			sys.exit(0)
else:
	ORIGINAL_OUTPUT_FILE_NAME=OUTPUT_FILE_NAME
	counter=1
	while os.path.exists(OUTPUT_FILE_NAME):
		split=os.path.splitext(ORIGINAL_OUTPUT_FILE_NAME)
		OUTPUT_FILE_NAME=split[0]+ str(counter) + split[1]
		counter+=1

if args.type == '1':
	print 'reading files...'
	data=XMLImportFluidTracking(args.xml_file)
	data.setImageFolder(args.image_folder)
	print 'writing to ' + OUTPUT_FILE_NAME + '...'
	data.exportToHDF5(OUTPUT_FILE_NAME)
elif args.type == '2':
	print 'reading files...'
	data=XMLImportFluidTrackingOld(args.label_folder, args.divisions_file, args.tracking_log)
	data.setImageFolder(args.image_folder)
	print 'writing to ' + OUTPUT_FILE_NAME + '...'
	data.exportToHDF5(OUTPUT_FILE_NAME)
elif args.type == '3':
	print 'reading files...'
	data=HDF5ImportIlastik(args.object_layer_file, args.tracking_project_file)
	data.setImageFolder(args.image_folder)
	print 'writing to ' + OUTPUT_FILE_NAME + '...'
	data.exportToHDF5(OUTPUT_FILE_NAME)
elif args.type == '4':
	print 'reading files...'
	data=XMLImportCelltracker(args.xml_folder, args.tracks_xml, args.some_image)
	data.setImageFolder(args.image_folder)
	print 'writing to ' + OUTPUT_FILE_NAME + '...'
	data.exportToHDF5(OUTPUT_FILE_NAME)

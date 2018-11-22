# -*- coding: utf-8 -*-
"""
Created on Thu Nov 22 09:33:24 2018

@author: juanferna.perez
"""

import matplotlib.pyplot as plt
import seaborn as sns; sns.set()
import numpy as np
from sklearn.metrics import pairwise_distances_argmin
from sklearn.datasets.samples_generator import make_blobs

def kmeans_paso_a_paso(X, n_clusters, num_pasos = 100, semilla=1):
    # inicializar los centroides de los clusters aleatoriamente
    rng = np.random.RandomState(semilla)
    i = rng.permutation(X.shape[0])[:n_clusters]
    centros = X[i]
    
    plt.figure()    
    plt.scatter(X[:,0], X[:,1], c='green', s=50)
    plt.scatter(centros[:,0], centros[:,1], c='black', s=200, alpha=0.5  )
    
    pasos = 0
    while True:
        # asignar etiquetas al centroide más cercano
        etiquetas = pairwise_distances_argmin(X,centros)
        plt.figure()    
        plt.scatter(X[:,0], X[:,1], c=etiquetas, s=50, cmap='viridis')
        plt.scatter(centros[:,0], centros[:,1], c='black', s=200, alpha=0.5  )
        
        if pasos == num_pasos:
            break
        
        
        # determinar nuevos centros como el punto medio de los puntos de cada cluster
        nuevos_centros = np.zeros((n_clusters,X.shape[1]))
        for i in range(n_clusters):
            nuevos_centros[i,:] = X[etiquetas==i].mean(0)
            
        plt.figure()    
        plt.scatter(X[:,0], X[:,1], c=etiquetas, s=50, cmap='viridis')
        plt.scatter(nuevos_centros[:,0], nuevos_centros[:,1], c='black', s=200, alpha=0.5  )
        
        
        #revisar convergencia
        if np.all(centros == nuevos_centros):
            break
        
        # actualizar centroides
        centros = nuevos_centros
        pasos = pasos + 1
        
    return centros, etiquetas

X, y = make_blobs(n_samples=300, centers=4, cluster_std=0.6, random_state=1)
plt.figure()    
plt.scatter(X[:,0], X[:,1], c='green', s=50)


#plt.scatter(X[:,0], X[:,1], s=50)
cents, etiqs = kmeans_paso_a_paso(X,4,3,100)
#cents, etiqs = kmeans_paso_a_paso(X,4,3,2)

#cents, etiqs = kmeans_paso_a_paso(X,4,1,100)
#plt.scatter(X[:,0], X[:,1], c=etiqs, s=50, cmap='viridis')
#plt.scatter(cents[:,0], cents[:,1], c='black', s=200, alpha=0.5  )
        
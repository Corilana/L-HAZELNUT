from multiprocessing import Pool
import lpy_simulation as lpy

#run 1000 simulations using 7 processors
if __name__ == "__main__":
    with Pool(processes = 7) as pool:
        pool.map(lpy.nbsimulate, range(0,1000))
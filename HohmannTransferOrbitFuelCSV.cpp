#include <iostream>
#include <cmath>
#include <vector>
#include <fstream>

using namespace std;



double ACCMag(double m2, double x1, double x2, double y1, double y2, double z1, double z2) {
    double G = 6.6743 * pow(10, -11);
    double mag = (G * m2) / (pow(x1 - x2, 2) + pow(y1 - y2, 2) + pow(z1 - z2, 2));
    return mag;
}

double Cosine(double x1, double x2, double y1, double y2) {
    return (x1 - x2) / sqrt(pow(x1 - x2, 2) + pow(y1 - y2, 2));
}

double Sine(double x1, double x2, double y1, double y2) {
    return (y1 - y2) / sqrt(pow(x1 - x2, 2) + pow(y1 - y2, 2));
}

double SinPhi(double x1, double x2, double y1, double y2, double z1, double z2) {
    return (z1 - z2) / sqrt(pow(x1 - x2, 2) + pow(y1 - y2, 2) + pow(z1 - z2, 2));
}

double CosPhi(double x1, double x2, double y1, double y2, double z1, double z2) {
    return sqrt(pow(x1 - x2, 2) + pow(y1 - y2, 2)) / sqrt(pow(x1 - x2, 2) + pow(y1 - y2, 2) + pow(z1 - z2, 2));
}

double FORCE_X(double m2, double x1, double x2, double y1, double y2, double z1, double z2) {
    return Cosine(x1, x2, y1, y2) * ACCMag(m2, x1, x2, y1, y2, z1, z2) * CosPhi(x1, x2, y1, y2, z1, z2);
}

double FORCE_Y(double m2, double x1, double x2, double y1, double y2, double z1, double z2) {
    return Sine(x1, x2, y1, y2) * ACCMag(m2, x1, x2, y1, y2, z1, z2) * CosPhi(x1, x2, y1, y2, z1, z2);
}

double FORCE_Z(double m2, double x1, double x2, double y1, double y2, double z1, double z2) {
    return SinPhi(x1, x2, y1, y2, z1, z2) * ACCMag(m2, x1, x2, y1, y2, z1, z2);
}

int main(){
    double G = 6.6743e-11;

    double M1 = 1.9891e30;
    double X1 = 0, Vx1 = 0, Ax1 = 0, Y1 = 0, Vy1 = 0, Ay1 = 0, Z1 = 0, Vz1 = 0, Az1=0;

    double M2 = 5.972e24;
    double X2 = 1.4960e11, Vx2 = 0, Ax2 = 0, Y2 = 0, Vy2 = 29.78e3, Ay2 = 0, Z2 = 0, Vz2 = 0, Az2=0;

    double M3 = 3.285e23;
    double X3 = 61.115e9, Vx3 = 0, Vy3 = 47870, Ax3 = 0, Ay3 = 0, Y3 = 0, Z3 = 0, Vz3 = 0, Az3=0;

    double M4 = 4.867e24;
    double X4 = 107.89e9, Vx4 = 0, Vy4 = 35020, Ax4 = 0, Ay4 = 0, Y4 = 0, Z4 = 0, Vz4 = 0, Az4=0;

    double M5 = 6.39e23;
    double X5 = 245.68e9, Vx5 = 0, Vy5 = 24077, Ax5 = 0, Ay5 = 0,Y5 = 0, Z5 = 0, Vz5 = 0, Az5=0;

    double M6 = 1000;
    double X6 = 1.4960e11+6371e3+2000e5, Vx6 = 0, Ax6 = 0, Y6 = 0, Vy6 = 29.78e3+1200, Ay6 = 0, Z6 = 0, Vz6 = 0, Az6=0; // Earth + Being very far out (being too close to earth cause many problems)

        double detT = 10;
        int N = 8000000;
        int a = 0;
        int Fuel = 0;

        int Storage = N/4000+1;

        double* X1Pos = new double[Storage]; // New way to store memory, I was running into memory errors unless I used pointers like this
        double* Y1Pos = new double[Storage];
        double* Z1Pos = new double[Storage];
        double* X2Pos = new double[Storage];
        double* Y2Pos = new double[Storage];
        double* Z2Pos = new double[Storage];
        double* X3Pos = new double[Storage];
        double* Y3Pos = new double[Storage]; 
        double* Z3Pos = new double[Storage];
        double* X4Pos = new double[Storage];
        double* Y4Pos = new double[Storage];
        double* Z4Pos = new double[Storage];
        double* X5Pos = new double[Storage];
        double* Y5Pos = new double[Storage];
        double* Z5Pos = new double[Storage];
        double* X6Pos = new double[Storage];
        double* Y6Pos = new double[Storage];
        double* Z6Pos = new double[Storage];
        double* fuel = new double[N];
        fuel[0]=0;


    double boost = 3.5;

    for(int i=0; i<=N; i++){
  

        Vx1 += Ax1*detT/2; //Half Step
        Vy1 += Ay1*detT/2; 
        Vz1 += Az1*detT/2; 
        Vx2 += Ax2*detT/2; 
        Vy2 += Ay2*detT/2; 
        Vz2 += Az2*detT/2; 
        Vx3 += Ax3*detT/2; 
        Vy3 += Ay3*detT/2; 
        Vz3 += Az3*detT/2; 
        Vx4 += Ax4*detT/2; //Half Step
        Vy4 += Ay4*detT/2; 
        Vz4 += Az4*detT/2; 
        Vx5 += Ax5*detT/2; 
        Vy5 += Ay5*detT/2; 
        Vz5 += Az5*detT/2; 
        Vx6 += Ax6*detT/2; 
        Vy6 += Ay6*detT/2; 
        Vz6 += Az6*detT/2; 
        
        X1 += Vx1*detT;
        Y1 += Vy1*detT;
        Z1 += Vz1*detT;
        X2 += Vx2*detT;
        Y2 += Vy2*detT;
        Z2 += Vz2*detT;
        X3 += Vx3*detT;
        Y3 += Vy3*detT;
        Z3 += Vz3*detT;
        X4 += Vx4*detT;
        Y4 += Vy4*detT;
        Z4 += Vz4*detT;
        X5 += Vx5*detT;
        Y5 += Vy5*detT;
        Z5 += Vz5*detT;
        X6 += Vx6*detT;
        Y6 += Vy6*detT;
        Z6 += Vz6*detT;
        //accelerations from sun to planets
        double SunF1x = FORCE_X(M1,X1,X3,Y1,Y3,Z1,Z3); //mercury force x
        double SunF1y = FORCE_Y(M1,X1,X3,Y1,Y3,Z1,Z3); //mercury force y
        double SunF1z = FORCE_Z(M1,X1,X3,Y1,Y3,Z1,Z3); //mercury force z

        double SunF2x = FORCE_X(M1,X1,X2,Y1,Y2,Z1,Z2); //earth force x
        double SunF2y = FORCE_Y(M1,X1,X2,Y1,Y2,Z1,Z2); //earth force y
        double SunF2z = FORCE_Z(M1,X1,X2,Y1,Y2,Z1,Z2); //earth force z

        double SunF3x = FORCE_X(M1,X1,X4,Y1,Y4,Z1,Z4); //venus force x
        double SunF3y = FORCE_Y(M1,X1,X4,Y1,Y4,Z1,Z4); //venus force y
        double SunF3z = FORCE_Z(M1,X1,X4,Y1,Y4,Z1,Z4); //venus force z

        double SunF4x = FORCE_X(M1,X1,X5,Y1,Y5,Z1,Z5); //mars force x
        double SunF4y = FORCE_Y(M1,X1,X5,Y1,Y5,Z1,Z5); //mars force y
        double SunF4z = FORCE_Z(M1,X1,X5,Y1,Y5,Z1,Z5); //mars force z

        double SunF5x = FORCE_X(M1,X1,X6,Y1,Y6,Z1,Z6); //Satellite force x (Sun)
        double SunF5y = FORCE_Y(M1,X1,X6,Y1,Y6,Z1,Z6); //Satellite force y
        double SunF5z = FORCE_Z(M1,X1,X6,Y1,Y6,Z1,Z6); //Satellite force z

        double MercF5x = FORCE_X(M3,X3,X6,Y3,Y6,Z3,Z6); //Satellite force x (Merc)
        double MercF5y = FORCE_Y(M3,X3,X6,Y3,Y6,Z3,Z6); //Satellite force y
        double MercF5z = FORCE_Z(M3,X3,X6,Y3,Y6,Z3,Z6); //Satellite force z

        double EarthF5x = FORCE_X(M2,X2,X6,Y2,Y6,Z2,Z6); //Satellite force x (Earth)
        double EarthF5y = FORCE_Y(M2,X2,X6,Y2,Y6,Z2,Z6); //Satellite force y
        double EarthF5z = FORCE_Z(M2,X2,X6,Y2,Y6,Z2,Z6); //Satellite force z

        double VenF5x = FORCE_X(M4,X4,X6,Y4,Y6,Z4,Z6); //Satellite force x (Venus)
        double VenF5y = FORCE_Y(M4,X4,X6,Y4,Y6,Z4,Z6); //Satellite force y
        double VenF5z = FORCE_Z(M4,X4,X6,Y4,Y6,Z4,Z6); //Satellite force z

        double MarsF5x = FORCE_X(M5,X5,X6,Y5,Y6,Z5,Z6); //Satellite force x (Mars)
        double MarsF5y = FORCE_Y(M5,X5,X6,Y5,Y6,Z5,Z6); //Satellite force y
        double MarsF5z = FORCE_Z(M5,X5,X6,Y5,Y6,Z5,Z6); //Satellite force z
        
        
        Ax1 = -SunF1x*M3/M1 - SunF2x*M2/M1 -SunF3x*M4/M1 - SunF4x*M5/M1; // Newton's Third Law
        Ay1 = -SunF1y*M3/M1 - SunF2y*M2/M1 -SunF3y*M4/M1 - SunF4y*M5/M1;
        Az1 = -SunF1z*M3/M1 - SunF2z*M2/M1 -SunF3z*M4/M1 - SunF4z*M5/M1;
        
        Ax2 = SunF2x; //earth
        Ay2 = SunF2y;
        Az2 = SunF2z;
        
        Ax3 = SunF1x; //mercury
        Ay3 = SunF1y; 
        Az3 = SunF1z;
        
        Ax4 = SunF3x;
        Ay4 = SunF3y;
        Az4 = SunF3z; //venus
        
        Ax5 = SunF4x; //mars
        Ay5 = SunF4y;
        Az5 = SunF4z;

        if(i < N/20){ // Double if statement -> if 1 = start of sim, if 2 -> accelerate to needed det V in the correct direction too
            if(sqrt(pow(Vx6,2)+pow(Vy6,2)+pow(Vz6,2)) < 32680){
            Ax6 = SunF5x+MercF5x+EarthF5x+VenF5x+MarsF5x+boost*Vx6/sqrt(pow(Vx6,2)+pow(Vy6,2)+pow(Vz6,2)); //Sattelite
            Ay6 = SunF5y+MercF5y+EarthF5y+VenF5y+MarsF5y+boost*Vy6/sqrt(pow(Vx6,2)+pow(Vy6,2)+pow(Vz6,2));
            Az6 = SunF5z+MercF5z+EarthF5z+VenF5z+MarsF5z+boost*Vz6/sqrt(pow(Vx6,2)+pow(Vy6,2)+pow(Vz6,2));
            fuel[i+1] = fuel[i] + 1;
            cout << fuel[i] << endl;
            }
            else{
            Ax6 = SunF5x+MercF5x+EarthF5x+VenF5x+MarsF5x; //Sattelite
            Ay6 = SunF5y+MercF5y+EarthF5y+VenF5y+MarsF5y;
            Az6 = SunF5z+MercF5z+EarthF5z+VenF5z+MarsF5z;
            fuel[i+1] = fuel[i];
            }

        }
        else{
        Ax6 = SunF5x+MercF5x+EarthF5x+VenF5x+MarsF5x; //Sattelite
        Ay6 = SunF5y+MercF5y+EarthF5y+VenF5y+MarsF5y;
        Az6 = SunF5z+MercF5z+EarthF5z+VenF5z+MarsF5z;
        fuel[i+1] = fuel[i];
        }
        Vx1 += Ax1*detT/2; //Half Step
        Vy1 += Ay1*detT/2; 
        Vz1 += Az1*detT/2; 
        Vx2 += Ax2*detT/2; 
        Vy2 += Ay2*detT/2; 
        Vz2 += Az2*detT/2; 
        Vx3 += Ax3*detT/2; 
        Vy3 += Ay3*detT/2; 
        Vz3 += Az3*detT/2; 
        Vx4 += Ax4*detT/2; //Half Step
        Vy4 += Ay4*detT/2; 
        Vz4 += Az4*detT/2; 
        Vx5 += Ax5*detT/2; 
        Vy5 += Ay5*detT/2; 
        Vz5 += Az5*detT/2;
        Vx6 += Ax6*detT/2; 
        Vy6 += Ay6*detT/2; 
        Vz6 += Az6*detT/2; 
        if(i%40000 == 0){
            
            cout << i << "  " << a << endl;

            X1Pos[a] = X1;
            Y1Pos[a] = Y1;
            Z1Pos[a] = Z1;
            X2Pos[a] = X2;
            Y2Pos[a] = Y2;
            Z2Pos[a] = Z2;
            X3Pos[a] = X3;
            Y3Pos[a] = Y3;
            Z3Pos[a] = Z3;
            X4Pos[a] = X4;
            Y4Pos[a] = Y4;
            Z4Pos[a] = Z4;
            X5Pos[a] = X5;
            Y5Pos[a] = Y5;
            Z5Pos[a] = Z5;
            X6Pos[a] = X6;
            Y6Pos[a] = Y6;
            Z6Pos[a] = Z6;    
            a = a+1;    
        }
    }

    ofstream file;
    file.open("solarsystem.csv");
    for (int j = 0; j <= a-1; j++){
        file << X1Pos[j] << "," << Y1Pos[j]<< "," << Z1Pos[j]<< "," << X2Pos[j] << "," << Y2Pos[j]<< ","  << Z2Pos[j]<< "," << X3Pos[j] << "," << Y3Pos[j]<< "," << Z3Pos[j]<<","<< X4Pos[j] << "," << Y4Pos[j]<< "," << Z4Pos[j]<<","<< X5Pos[j] << "," << Y5Pos[j]<< "," << Z5Pos[j] << "," << X6Pos[j]<< "," << Y6Pos[j]<< "," << Z6Pos[j] <<"\n";
    }
    file.close();
    cout << "Fuel Cost" << Fuel << endl;


    ofstream file1;
    file1.open("fuel1.csv");
    for (int j = 0; j < N/4; j++){
        file1 << fuel[4*j] << "\n";
    }
    file1.close();


    delete[] X1Pos;
    delete[] Y1Pos;
    delete[] Z1Pos;
    delete[] X2Pos;
    delete[] Y2Pos;
    delete[] Z2Pos;
    delete[] X3Pos;
    delete[] Y3Pos;
    delete[] Z3Pos;
    delete[] X4Pos;
    delete[] Y4Pos;
    delete[] Z4Pos;
    delete[] X5Pos;
    delete[] Y5Pos;
    delete[] Z5Pos;
    delete[] X6Pos;
    delete[] Y6Pos;
    delete[] Z6Pos;
    delete[] fuel;
}

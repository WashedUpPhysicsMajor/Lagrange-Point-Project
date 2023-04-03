#include <iostream>
#include <cmath>
#include <vector>

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

int main() {
    double G = 6.6743 * pow(10, -11.);
    double M1 = 1.9891 * pow(10, 30.);
    double X1 = 0;
    double Vx1 = 0;
    double Ax1 = 0;
    double Y1 = 0;
    double Vy1 = 0;
    double Ay1 = 0;
    double Z1 = 0;
    double Vz1 = 0;
    double Az1 = 0;

    double M2 = 5.972 * pow(10, 24.);
    double X2 = 1.4960 * pow(10,11.);
    double Vx2 = 0;
    double Ax2 = 0;
    double Y2 = 0;
    double Vy2 = 29.78 * pow(10,3.);
    double Ay2 = 0;
    double Z2 = 0;
    double Vz2 = 0;
    double Az2 = 0;

    double M3 = 6200;
    double X3 = 1.4960 * pow(10,11.)+1500000000;
    double Vx3 = 0;
    double Ax3 = 0;
    double Y3 = -800000000;
    double Vy3 = 30.2 * pow(10,3.);
    double Ay3 = 0;
    double Z3 = 0;
    double Vz3 = -500;
    double Az3 = 0;

    double detT = 10;

    int N = 3.154 * pow(10,6.);

    int a = 0;

    double ratio = X3/X2;

    int count = 0;

    double C[N/4000];

    double X1Pos[N/4000];
    double Y1Pos[N/4000];
    double Z1Pos[N/4000];
    double X2Pos[N/4000];
    double Y2Pos[N/4000];
    double Z2Pos[N/4000];
    double X3Pos[N/4000];
    double Y3Pos[N/4000]; 
    double Z3Pos[N/4000];

    double boost = 0.0025*30;

    for(int i=0; i < N; i++) {                    
    Vx1 = Vx1+Ax1*detT/2.;
    Vy1 = Vy1+Ay1*detT/2.;
    Vz1 = Vz1+Az1*detT/2.; 
    Vx2 = Vx2+Ax2*detT/2.; 
    Vy2 = Vy2+Ay2*detT/2.; 
    Vz2 = Vz2+Az2*detT/2.; 
    Vx3 = Vx3+Ax3*detT/2.; 
    Vy3 = Vy3+Ay3*detT/2.;
    Vz3 = Vz3+Az3*detT/2.;
    X1 = X1+Vx1*detT;
    Y1 = Y1+Vy1*detT;
    Z1 = Z1+Vz1*detT;
    X2 = X2+Vx2*detT;
    Y2 = Y2+Vy2*detT;
    Z2 = Z2+Vz2*detT;
    X3 = X3+Vx3*detT;
    Y3 = Y3+Vy3*detT;
    Z3 = Z3+Vz3*detT;
    
    if (pow(pow((ratio*X2-X3),2.)+pow((ratio*Y2-Y3),2.)+pow(Z3,2.),(0.5)) > 800000000){
        Ax3 = FORCE_X(M2, X2, X3, Y2, Y3,Z2,Z3)+ FORCE_X(M1, X1, X3, Y1, Y3,Z1,Z3) + boost*(ratio*X2-X3)/(pow(pow((ratio*X2-X3),2.)+pow((ratio*Y2-Y3),2.)+pow(Z3,2.),0.5));
        Ay3 = FORCE_Y(M2, X2, X3, Y2, Y3,Z2,Z3)+ FORCE_Y(M1, X1, X3, Y1, Y3,Z1,Z3) + boost*(ratio*Y2-Y3)/(pow(pow((ratio*X2-X3),2.)+pow((ratio*Y2-Y3),2.)+pow(Z3,2.),0.5));
        Az3 = FORCE_Z(M2, X2, X3, Y2, Y3,Z2,Z3)+ FORCE_Z(M1, X1, X3, Y1, Y3,Z1,Z3) + boost*(-Z3)/(pow(pow((ratio*X2-X3),2.)+pow((ratio*Y2-Y3),2.)+pow(Z3,2.),0.5));
        count = count+1;
    }
    else{
        Ax3 = FORCE_X(M2, X2, X3, Y2, Y3,Z2,Z3)+ FORCE_X(M1, X1, X3, Y1, Y3,Z1,Z3);
        Ay3 = FORCE_Y(M2, X2, X3, Y2, Y3,Z2,Z3)+ FORCE_Y(M1, X1, X3, Y1, Y3,Z1,Z3);
        Az3 = FORCE_Z(M2, X2, X3, Y2, Y3,Z2,Z3)+ FORCE_Z(M1, X1, X3, Y1, Y3,Z1,Z3);
    }

    Ax1 = -FORCE_X(M2, X1, X2, Y1, Y2,Z1,Z2) - FORCE_X(M3, X1, X3, Y1, Y3,Z1,Z3);
    Ay1 = -FORCE_Y(M2, X1, X2, Y1, Y2,Z1,Z2) - FORCE_Y(M3, X1, X3, Y1, Y3,Z1,Z3);
    Az1 = -FORCE_Z(M2, X1, X2, Y1, Y2,Z1,Z2) - FORCE_Z(M3, X1, X3, Y1, Y3,Z1,Z3);
    Ax2 = FORCE_X(M1, X1, X2, Y1, Y2,Z1,Z2) - FORCE_X(M3, X2, X3, Y2, Y3,Z2,Z3);
    Ay2 = FORCE_Y(M1, X1, X2, Y1, Y2,Z1,Z2) - FORCE_Y(M3, X2, X3, Y2, Y3,Z2,Z3);
    Az2 = FORCE_Z(M1, X1, X2, Y1, Y2,Z1,Z2) - FORCE_Z(M3, X2, X3, Y2, Y3,Z2,Z3);
    
    Vx1 = Vx1+Ax1*detT/2.;
    Vy1 = Vy1+Ay1*detT/2.;
    Vz1 = Vz1+Az1*detT/2.;
    Vx2 = Vx2+Ax2*detT/2.;
    Vy2 = Vy2+Ay2*detT/2.;
    Vz2 = Vz2+Az2*detT/2.;
    Vx3 = Vx3+Ax3*detT/2.;
    Vy3 = Vy3+Ay3*detT/2.;
    Vz3 = Vz3+Az3*detT/2.;
    
    if(i%4000 == 0){
        a = a+1;
        X1Pos[a] = X1;
        Y1Pos[a] = Y1;
        Z1Pos[a] = Z1;
        X2Pos[a] = X2;
        Y2Pos[a] = Y2;
        Z2Pos[a] = Z2;
        X3Pos[a] = X3;
        Y3Pos[a] = Y3;
        Z3Pos[a] = Z3;
        C[a] = count;
    }
    
    
    }
    cout.precision(17);
    cout << "The total Fuel Spent is: " << C[a]*boost << endl;
    }

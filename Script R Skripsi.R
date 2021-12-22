#Menentukan Banyak Pengulangan Simulasi
Rho = c(0,1000)
L   = c(0,1000)
Lq  = c(0,1000)
W   = c(0,1000)
Wq  = c(0,1000)
for (u in 1:1000){

#Membuat Awalan Simulasi Sistem Antrian (Proses Inisialisasi)
InSys      = 0
TM         = 0
SS         = 0
WL         = 0
AT         = 0
DT         = 9999
MX1        = 225
MX2        = 240
N          = 0
SigBESS    = 0
SigBEInSys = 0
SigBEWL    = 0
Lambda     = 0.1167
b          = 2

#Membuat Algoritma Simulasi Sistem Antrian
while (TM < MX1) 
  {Z = TM
   if (AT < DT)
      {EventType = "Arrival"
       R1        = runif(1)
                   if (R1 <= 0.70) {GS = 1} 
                      else if (R1 <= 0.96) {GS = 2}
                              else {GS = 3}
       TM        = AT
       InSys     = InSys + GS
       if (SS == 0)
          {SS = 1
           ST = rlnorm3 (1, 1.0082, 0.3103, 0)
           DT = TM+ST} 
          else {WL = WL + GS}
       IT = rgevd(1, 1.5149, 1.5149, -0.6708)
       AT = TM + IT} 
      else {EventType = "Departure"
            TM        = DT
            InSys     = InSys - 1
            if (WL > 0)
               {WL  = WL - 1
                ST  = rlnorm3 (1, 1.0082, 0.3103, 0)
                DT  = TM + ST}
               else {SS = 0
                     DT = 9999}}
  
BE         = TM - Z
BESS       = BE * SS
SigBESS    = SigBESS + BESS
BEInSys    = BE * InSys
SigBEInSys = SigBEInSys + BEInSys
BEWL       = BE * WL
SigBEWL    = SigBEWL + BEWL

if (N == 0)			   
   {A = c ('n','EventType','IT','ST','#InSys','TM','SS','WL','AT','DT','BE','BESS','BEInSys','BEWL')}
   B = c (N, EventType, IT, ST, InSys, TM, SS, WL, AT, DT, BE, BESS, BEInSys, BEWL)
   N = N + 1}

#Menghitung Nilai Kinerja Sistem Antrian
Rho [u] = SigBESS/MX2
L   [u] = SigBEInSys/MX2
Lq  [u] = SigBEWL/MX2
W   [u] = L[u]/(Lambda*b)
Wq  [u] = Lq[u]/(Lambda*b)}
Rho
L
Lq
W
Wq

#Menghitung Rata-Rata Nilai Kinerja Sistem Antrian dari Hasil Pengulangan
RataRataRho = mean (Rho)
RataRataL   = mean (L)
RataRataLq  = mean (Lq)
RataRataW   = mean (W)
RataRataWq  = mean (Wq)
RataRataRho
RataRataL
RataRataLq
RataRataW
RataRataWq

#Menghitung Standar Deviasi Nilai Kinerja Sistem Antrian dari Hasil Pengulangan
StDevRho = sd (Rho)
StDevL   = sd (L)
StDevLq  = sd (Lq)
StDevW   = sd (W)
StDevWq  = sd (Wq)
StDevRho
StDevL
StDevLq
StDevW
StDevWq

#Menghitung Selang Kepercayaan Nilai Kinerja Sistem Antrian dari Hasil Pengulangan
l.Rho = lm (Rho ~ 1)
l.L   = lm (L ~ 1)
l.Lq  = lm (Lq ~ 1)
l.W   = lm (W ~ 1)
l.Wq  = lm (Wq ~ 1)
SKRho = confint(l.Rho, level = 0.95)
SKL   = confint(l.L, level = 0.95)
SKLq  = confint(l.Lq, level = 0.95)
SKW   = confint(l.W, level = 0.95)
SKWq  = confint(l.Wq, level = 0.95)
SKRho
SKL
SKLq
SKW
SKWq
#define AESx(X)     \
    IOR(IOR(IOR(IAND(SHIFTR(ToInteger(X), 24), ToInteger(Z'000000FF')),  \
                IAND(SHIFTR(ToInteger(X),  8), ToInteger(Z'0000FF00'))), \
                IAND(SHIFTL(ToInteger(X),  8), ToInteger(Z'00FF0000'))), \
                IAND(SHIFTL(ToInteger(X), 24), ToInteger(Z'FF000000')))
#define AES0        AES0_BE
#define AES1        AES1_BE
#define AES2        AES2_BE
#define AES3        AES3_BE
#define Mask(X)     IAND(X, ToInteger(Z'000000FF'))
#define AES_Assign(X0, X1, X2, X3, K)   \
    IEOR(IEOR(IEOR(IEOR(AES0(Mask(SHIFTR(X0, 24))), AES1(Mask(SHIFTR(X1, 16)))), AES2(Mask(SHIFTR(X2, 8)))), AES3(Mask(X3))), K)
#define AES_ROUND_BE(X0, X1, X2, X3, K0, K1, K2, K3, Y0, Y1, Y2, Y3)    \
    Y0 = AES_Assign(X0, X1, X2, X3, K0); \
    Y1 = AES_Assign(X1, X2, X3, X0, K1); \
    Y2 = AES_Assign(X2, X3, X0, X1, K2); \
    Y3 = AES_Assign(X3, X0, X1, X2, K3);
#define AES_ROUND_NOKEY_BE(X0, X1, X2, X3, Y0, Y1, Y2, Y3)  AES_ROUND_BE(X0, X1, X2, X3, 0, 0, 0, 0, Y0, Y1, Y2, Y3)

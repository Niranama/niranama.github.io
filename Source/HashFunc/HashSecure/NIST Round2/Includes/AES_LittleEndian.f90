#define AESx(X)     ToInteger(X)
#define AES0        AES0_LE
#define AES1        AES1_LE
#define AES2        AES2_LE
#define AES3        AES3_LE
#define Mask(X)     IAND(X, ToInteger(Z'000000FF'))
#define AES_Assign(X0, X1, X2, X3, K)   \
    IEOR(IEOR(IEOR(IEOR(AES0(Mask(X0)), AES1(Mask(SHIFTR(X1, 8)))), AES2(Mask(SHIFTR(X2, 16)))), AES3(Mask(SHIFTR(X3, 24)))), K)
#define AES_ROUND_LE(X0, X1, X2, X3, K0, K1, K2, K3, Y0, Y1, Y2, Y3)    \
    Y0 = AES_Assign(X0, X1, X2, X3, K0); \
    Y1 = AES_Assign(X1, X2, X3, X0, K1); \
    Y2 = AES_Assign(X2, X3, X0, X1, K2); \
    Y3 = AES_Assign(X3, X0, X1, X2, K3);
#define AES_ROUND_NOKEY_LE(X0, X1, X2, X3, Y0, Y1, Y2, Y3)  AES_ROUND_LE(X0, X1, X2, X3, 0, 0, 0, 0, Y0, Y1, Y2, Y3)

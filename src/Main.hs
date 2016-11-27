{-# LANGUAGE QuasiQuotes, RankNTypes, TemplateHaskell #-}
module Main where
import Data.Word
import Foreign
import qualified Language.C.Inline as C
C.include "<stdio.h>"

main :: IO ()
main = do
  bytePtr <- mallocBytes 4
  let rawPtr :: forall a. Ptr a
      rawPtr = castPtr bytePtr
  poke rawPtr (65535 :: Word32)
  
  bytes <- mapM (peekElemOff rawPtr) [0..3]
  print (bytes :: [Word8])
  
  [C.block| void {
    unsigned char *p = $(unsigned char *rawPtr);
    for(int i=0; i<4; ++i) printf("%d\n", p[i]);
  }|]
  
  free rawPtr

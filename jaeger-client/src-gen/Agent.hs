{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-----------------------------------------------------------------
-- Autogenerated by Thrift Compiler (0.11.0)                      --
--                                                             --
-- DO NOT EDIT UNLESS YOU ARE SURE YOU KNOW WHAT YOU ARE DOING --
-----------------------------------------------------------------

module Agent where
import Prelude (($), (.), (>>=), (==), (++))
import qualified Prelude as P
import qualified Control.Exception as X
import qualified Control.Monad as M ( liftM, ap, when )
import Data.Functor ( (<$>) )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Hashable as H
import qualified Data.Int as I
import qualified Data.Maybe as M (catMaybes)
import qualified Data.Text.Lazy.Encoding as E ( decodeUtf8, encodeUtf8 )
import qualified Data.Text.Lazy as LT
import qualified GHC.Generics as G (Generic)
import qualified Data.Typeable as TY ( Typeable )
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Vector as Vector
import qualified Test.QuickCheck.Arbitrary as QC ( Arbitrary(..) )
import qualified Test.QuickCheck as QC ( elements )

import qualified Thrift as T
import qualified Thrift.Types as T
import qualified Thrift.Arbitraries as T

import qualified Jaeger_Types
import qualified Zipkincore_Types


import Agent_Types
import qualified Agent_Iface as Iface
-- HELPER FUNCTIONS AND STRUCTURES --

data EmitZipkinBatch_args = EmitZipkinBatch_args  { emitZipkinBatch_args_spans :: (Vector.Vector Zipkincore_Types.Span)
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable EmitZipkinBatch_args where
  hashWithSalt salt record = salt   `H.hashWithSalt` emitZipkinBatch_args_spans record  
instance QC.Arbitrary EmitZipkinBatch_args where 
  arbitrary = M.liftM EmitZipkinBatch_args (QC.arbitrary)
  shrink obj | obj == default_EmitZipkinBatch_args = []
             | P.otherwise = M.catMaybes
    [ if obj == default_EmitZipkinBatch_args{emitZipkinBatch_args_spans = emitZipkinBatch_args_spans obj} then P.Nothing else P.Just $ default_EmitZipkinBatch_args{emitZipkinBatch_args_spans = emitZipkinBatch_args_spans obj}
    ]
from_EmitZipkinBatch_args :: EmitZipkinBatch_args -> T.ThriftVal
from_EmitZipkinBatch_args record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v2 -> P.Just (1, ("spans",T.TList (T.T_STRUCT Zipkincore_Types.typemap_Span) $ P.map (\_v4 -> Zipkincore_Types.from_Span _v4) $ Vector.toList _v2))) $ emitZipkinBatch_args_spans record
  ]
write_EmitZipkinBatch_args :: T.Protocol p => p -> EmitZipkinBatch_args -> P.IO ()
write_EmitZipkinBatch_args oprot record = T.writeVal oprot $ from_EmitZipkinBatch_args record
encode_EmitZipkinBatch_args :: T.StatelessProtocol p => p -> EmitZipkinBatch_args -> LBS.ByteString
encode_EmitZipkinBatch_args oprot record = T.serializeVal oprot $ from_EmitZipkinBatch_args record
to_EmitZipkinBatch_args :: T.ThriftVal -> EmitZipkinBatch_args
to_EmitZipkinBatch_args (T.TStruct fields) = EmitZipkinBatch_args{
  emitZipkinBatch_args_spans = P.maybe (emitZipkinBatch_args_spans default_EmitZipkinBatch_args) (\(_,_val6) -> (case _val6 of {T.TList _ _val7 -> (Vector.fromList $ P.map (\_v8 -> (case _v8 of {T.TStruct _val9 -> (Zipkincore_Types.to_Span (T.TStruct _val9)); _ -> P.error "wrong type"})) _val7); _ -> P.error "wrong type"})) (Map.lookup (1) fields)
  }
to_EmitZipkinBatch_args _ = P.error "not a struct"
read_EmitZipkinBatch_args :: T.Protocol p => p -> P.IO EmitZipkinBatch_args
read_EmitZipkinBatch_args iprot = to_EmitZipkinBatch_args <$> T.readVal iprot (T.T_STRUCT typemap_EmitZipkinBatch_args)
decode_EmitZipkinBatch_args :: T.StatelessProtocol p => p -> LBS.ByteString -> EmitZipkinBatch_args
decode_EmitZipkinBatch_args iprot bs = to_EmitZipkinBatch_args $ T.deserializeVal iprot (T.T_STRUCT typemap_EmitZipkinBatch_args) bs
typemap_EmitZipkinBatch_args :: T.TypeMap
typemap_EmitZipkinBatch_args = Map.fromList [(1,("spans",(T.T_LIST (T.T_STRUCT Zipkincore_Types.typemap_Span))))]
default_EmitZipkinBatch_args :: EmitZipkinBatch_args
default_EmitZipkinBatch_args = EmitZipkinBatch_args{
  emitZipkinBatch_args_spans = Vector.empty}
data EmitZipkinBatch_result = EmitZipkinBatch_result deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable EmitZipkinBatch_result where
  hashWithSalt salt record = salt  
instance QC.Arbitrary EmitZipkinBatch_result where 
  arbitrary = QC.elements [EmitZipkinBatch_result]
from_EmitZipkinBatch_result :: EmitZipkinBatch_result -> T.ThriftVal
from_EmitZipkinBatch_result record = T.TStruct $ Map.fromList $ M.catMaybes
  []
write_EmitZipkinBatch_result :: T.Protocol p => p -> EmitZipkinBatch_result -> P.IO ()
write_EmitZipkinBatch_result oprot record = T.writeVal oprot $ from_EmitZipkinBatch_result record
encode_EmitZipkinBatch_result :: T.StatelessProtocol p => p -> EmitZipkinBatch_result -> LBS.ByteString
encode_EmitZipkinBatch_result oprot record = T.serializeVal oprot $ from_EmitZipkinBatch_result record
to_EmitZipkinBatch_result :: T.ThriftVal -> EmitZipkinBatch_result
to_EmitZipkinBatch_result (T.TStruct fields) = EmitZipkinBatch_result{

  }
to_EmitZipkinBatch_result _ = P.error "not a struct"
read_EmitZipkinBatch_result :: T.Protocol p => p -> P.IO EmitZipkinBatch_result
read_EmitZipkinBatch_result iprot = to_EmitZipkinBatch_result <$> T.readVal iprot (T.T_STRUCT typemap_EmitZipkinBatch_result)
decode_EmitZipkinBatch_result :: T.StatelessProtocol p => p -> LBS.ByteString -> EmitZipkinBatch_result
decode_EmitZipkinBatch_result iprot bs = to_EmitZipkinBatch_result $ T.deserializeVal iprot (T.T_STRUCT typemap_EmitZipkinBatch_result) bs
typemap_EmitZipkinBatch_result :: T.TypeMap
typemap_EmitZipkinBatch_result = Map.fromList []
default_EmitZipkinBatch_result :: EmitZipkinBatch_result
default_EmitZipkinBatch_result = EmitZipkinBatch_result{
}
data EmitBatch_args = EmitBatch_args  { emitBatch_args_batch :: Jaeger_Types.Batch
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable EmitBatch_args where
  hashWithSalt salt record = salt   `H.hashWithSalt` emitBatch_args_batch record  
instance QC.Arbitrary EmitBatch_args where 
  arbitrary = M.liftM EmitBatch_args (QC.arbitrary)
  shrink obj | obj == default_EmitBatch_args = []
             | P.otherwise = M.catMaybes
    [ if obj == default_EmitBatch_args{emitBatch_args_batch = emitBatch_args_batch obj} then P.Nothing else P.Just $ default_EmitBatch_args{emitBatch_args_batch = emitBatch_args_batch obj}
    ]
from_EmitBatch_args :: EmitBatch_args -> T.ThriftVal
from_EmitBatch_args record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v17 -> P.Just (1, ("batch",Jaeger_Types.from_Batch _v17))) $ emitBatch_args_batch record
  ]
write_EmitBatch_args :: T.Protocol p => p -> EmitBatch_args -> P.IO ()
write_EmitBatch_args oprot record = T.writeVal oprot $ from_EmitBatch_args record
encode_EmitBatch_args :: T.StatelessProtocol p => p -> EmitBatch_args -> LBS.ByteString
encode_EmitBatch_args oprot record = T.serializeVal oprot $ from_EmitBatch_args record
to_EmitBatch_args :: T.ThriftVal -> EmitBatch_args
to_EmitBatch_args (T.TStruct fields) = EmitBatch_args{
  emitBatch_args_batch = P.maybe (emitBatch_args_batch default_EmitBatch_args) (\(_,_val19) -> (case _val19 of {T.TStruct _val20 -> (Jaeger_Types.to_Batch (T.TStruct _val20)); _ -> P.error "wrong type"})) (Map.lookup (1) fields)
  }
to_EmitBatch_args _ = P.error "not a struct"
read_EmitBatch_args :: T.Protocol p => p -> P.IO EmitBatch_args
read_EmitBatch_args iprot = to_EmitBatch_args <$> T.readVal iprot (T.T_STRUCT typemap_EmitBatch_args)
decode_EmitBatch_args :: T.StatelessProtocol p => p -> LBS.ByteString -> EmitBatch_args
decode_EmitBatch_args iprot bs = to_EmitBatch_args $ T.deserializeVal iprot (T.T_STRUCT typemap_EmitBatch_args) bs
typemap_EmitBatch_args :: T.TypeMap
typemap_EmitBatch_args = Map.fromList [(1,("batch",(T.T_STRUCT Jaeger_Types.typemap_Batch)))]
default_EmitBatch_args :: EmitBatch_args
default_EmitBatch_args = EmitBatch_args{
  emitBatch_args_batch = Jaeger_Types.default_Batch}
data EmitBatch_result = EmitBatch_result deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable EmitBatch_result where
  hashWithSalt salt record = salt  
instance QC.Arbitrary EmitBatch_result where 
  arbitrary = QC.elements [EmitBatch_result]
from_EmitBatch_result :: EmitBatch_result -> T.ThriftVal
from_EmitBatch_result record = T.TStruct $ Map.fromList $ M.catMaybes
  []
write_EmitBatch_result :: T.Protocol p => p -> EmitBatch_result -> P.IO ()
write_EmitBatch_result oprot record = T.writeVal oprot $ from_EmitBatch_result record
encode_EmitBatch_result :: T.StatelessProtocol p => p -> EmitBatch_result -> LBS.ByteString
encode_EmitBatch_result oprot record = T.serializeVal oprot $ from_EmitBatch_result record
to_EmitBatch_result :: T.ThriftVal -> EmitBatch_result
to_EmitBatch_result (T.TStruct fields) = EmitBatch_result{

  }
to_EmitBatch_result _ = P.error "not a struct"
read_EmitBatch_result :: T.Protocol p => p -> P.IO EmitBatch_result
read_EmitBatch_result iprot = to_EmitBatch_result <$> T.readVal iprot (T.T_STRUCT typemap_EmitBatch_result)
decode_EmitBatch_result :: T.StatelessProtocol p => p -> LBS.ByteString -> EmitBatch_result
decode_EmitBatch_result iprot bs = to_EmitBatch_result $ T.deserializeVal iprot (T.T_STRUCT typemap_EmitBatch_result) bs
typemap_EmitBatch_result :: T.TypeMap
typemap_EmitBatch_result = Map.fromList []
default_EmitBatch_result :: EmitBatch_result
default_EmitBatch_result = EmitBatch_result{
}
process_emitZipkinBatch (seqid, iprot, oprot, handler) = do
  args <- read_EmitZipkinBatch_args iprot
  (X.catch
    (do
      Iface.emitZipkinBatch handler (emitZipkinBatch_args_spans args)
      P.return ())
    ((\_ -> do
      P.return ()) :: X.SomeException -> P.IO ()))
process_emitBatch (seqid, iprot, oprot, handler) = do
  args <- read_EmitBatch_args iprot
  (X.catch
    (do
      Iface.emitBatch handler (emitBatch_args_batch args)
      P.return ())
    ((\_ -> do
      P.return ()) :: X.SomeException -> P.IO ()))
proc_ handler (iprot,oprot) (name,typ,seqid) = case name of
  "emitZipkinBatch" -> process_emitZipkinBatch (seqid,iprot,oprot,handler)
  "emitBatch" -> process_emitBatch (seqid,iprot,oprot,handler)
  _ -> do
    _ <- T.readVal iprot (T.T_STRUCT Map.empty)
    T.writeMessage oprot (name,T.M_EXCEPTION,seqid) $
      T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN_METHOD ("Unknown function " ++ LT.unpack name))
process handler (iprot, oprot) = do
  T.readMessage iprot (
    proc_ handler (iprot,oprot))
  P.return P.True

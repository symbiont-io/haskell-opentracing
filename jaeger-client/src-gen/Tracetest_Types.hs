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

module Tracetest_Types where
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


data Transport = HTTP|TCHANNEL|DUMMY  deriving (P.Show, P.Eq, G.Generic, TY.Typeable, P.Ord, P.Bounded)
instance P.Enum Transport where
  fromEnum t = case t of
    HTTP -> 0
    TCHANNEL -> 1
    DUMMY -> 2
  toEnum t = case t of
    0 -> HTTP
    1 -> TCHANNEL
    2 -> DUMMY
    _ -> X.throw T.ThriftException
instance H.Hashable Transport where
  hashWithSalt salt = H.hashWithSalt salt P.. P.fromEnum
instance QC.Arbitrary Transport where
  arbitrary = QC.elements (P.enumFromTo P.minBound P.maxBound)
data Downstream = Downstream  { downstream_serviceName :: LT.Text
  , downstream_serverRole :: LT.Text
  , downstream_host :: LT.Text
  , downstream_port :: LT.Text
  , downstream_transport :: Transport
  , downstream_downstream :: P.Maybe Downstream
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable Downstream where
  hashWithSalt salt record = salt   `H.hashWithSalt` downstream_serviceName record   `H.hashWithSalt` downstream_serverRole record   `H.hashWithSalt` downstream_host record   `H.hashWithSalt` downstream_port record   `H.hashWithSalt` downstream_transport record   `H.hashWithSalt` downstream_downstream record  
instance QC.Arbitrary Downstream where 
  arbitrary = M.liftM Downstream (QC.arbitrary)
          `M.ap`(QC.arbitrary)
          `M.ap`(QC.arbitrary)
          `M.ap`(QC.arbitrary)
          `M.ap`(QC.arbitrary)
          `M.ap`(M.liftM P.Just QC.arbitrary)
  shrink obj | obj == default_Downstream = []
             | P.otherwise = M.catMaybes
    [ if obj == default_Downstream{downstream_serviceName = downstream_serviceName obj} then P.Nothing else P.Just $ default_Downstream{downstream_serviceName = downstream_serviceName obj}
    , if obj == default_Downstream{downstream_serverRole = downstream_serverRole obj} then P.Nothing else P.Just $ default_Downstream{downstream_serverRole = downstream_serverRole obj}
    , if obj == default_Downstream{downstream_host = downstream_host obj} then P.Nothing else P.Just $ default_Downstream{downstream_host = downstream_host obj}
    , if obj == default_Downstream{downstream_port = downstream_port obj} then P.Nothing else P.Just $ default_Downstream{downstream_port = downstream_port obj}
    , if obj == default_Downstream{downstream_transport = downstream_transport obj} then P.Nothing else P.Just $ default_Downstream{downstream_transport = downstream_transport obj}
    , if obj == default_Downstream{downstream_downstream = downstream_downstream obj} then P.Nothing else P.Just $ default_Downstream{downstream_downstream = downstream_downstream obj}
    ]
from_Downstream :: Downstream -> T.ThriftVal
from_Downstream record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v2 -> P.Just (1, ("serviceName",T.TString $ E.encodeUtf8 _v2))) $ downstream_serviceName record
  , (\_v2 -> P.Just (2, ("serverRole",T.TString $ E.encodeUtf8 _v2))) $ downstream_serverRole record
  , (\_v2 -> P.Just (3, ("host",T.TString $ E.encodeUtf8 _v2))) $ downstream_host record
  , (\_v2 -> P.Just (4, ("port",T.TString $ E.encodeUtf8 _v2))) $ downstream_port record
  , (\_v2 -> P.Just (5, ("transport",T.TI32 $ P.fromIntegral $ P.fromEnum _v2))) $ downstream_transport record
  , (\_v2 -> (6, ("downstream",from_Downstream _v2))) <$> downstream_downstream record
  ]
write_Downstream :: T.Protocol p => p -> Downstream -> P.IO ()
write_Downstream oprot record = T.writeVal oprot $ from_Downstream record
encode_Downstream :: T.StatelessProtocol p => p -> Downstream -> LBS.ByteString
encode_Downstream oprot record = T.serializeVal oprot $ from_Downstream record
to_Downstream :: T.ThriftVal -> Downstream
to_Downstream (T.TStruct fields) = Downstream{
  downstream_serviceName = P.maybe (P.error "Missing required field: serviceName") (\(_,_val4) -> (case _val4 of {T.TString _val5 -> E.decodeUtf8 _val5; _ -> P.error "wrong type"})) (Map.lookup (1) fields),
  downstream_serverRole = P.maybe (P.error "Missing required field: serverRole") (\(_,_val4) -> (case _val4 of {T.TString _val6 -> E.decodeUtf8 _val6; _ -> P.error "wrong type"})) (Map.lookup (2) fields),
  downstream_host = P.maybe (P.error "Missing required field: host") (\(_,_val4) -> (case _val4 of {T.TString _val7 -> E.decodeUtf8 _val7; _ -> P.error "wrong type"})) (Map.lookup (3) fields),
  downstream_port = P.maybe (P.error "Missing required field: port") (\(_,_val4) -> (case _val4 of {T.TString _val8 -> E.decodeUtf8 _val8; _ -> P.error "wrong type"})) (Map.lookup (4) fields),
  downstream_transport = P.maybe (P.error "Missing required field: transport") (\(_,_val4) -> (case _val4 of {T.TI32 _val9 -> P.toEnum $ P.fromIntegral _val9; _ -> P.error "wrong type"})) (Map.lookup (5) fields),
  downstream_downstream = P.maybe (P.Nothing) (\(_,_val4) -> P.Just (case _val4 of {T.TStruct _val10 -> (to_Downstream (T.TStruct _val10)); _ -> P.error "wrong type"})) (Map.lookup (6) fields)
  }
to_Downstream _ = P.error "not a struct"
read_Downstream :: T.Protocol p => p -> P.IO Downstream
read_Downstream iprot = to_Downstream <$> T.readVal iprot (T.T_STRUCT typemap_Downstream)
decode_Downstream :: T.StatelessProtocol p => p -> LBS.ByteString -> Downstream
decode_Downstream iprot bs = to_Downstream $ T.deserializeVal iprot (T.T_STRUCT typemap_Downstream) bs
typemap_Downstream :: T.TypeMap
typemap_Downstream = Map.fromList [(1,("serviceName",T.T_STRING)),(2,("serverRole",T.T_STRING)),(3,("host",T.T_STRING)),(4,("port",T.T_STRING)),(5,("transport",T.T_I32)),(6,("downstream",(T.T_STRUCT typemap_Downstream)))]
default_Downstream :: Downstream
default_Downstream = Downstream{
  downstream_serviceName = "",
  downstream_serverRole = "",
  downstream_host = "",
  downstream_port = "",
  downstream_transport = (P.toEnum 0),
  downstream_downstream = P.Nothing}
data StartTraceRequest = StartTraceRequest  { startTraceRequest_serverRole :: LT.Text
  , startTraceRequest_sampled :: P.Bool
  , startTraceRequest_baggage :: LT.Text
  , startTraceRequest_downstream :: Downstream
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable StartTraceRequest where
  hashWithSalt salt record = salt   `H.hashWithSalt` startTraceRequest_serverRole record   `H.hashWithSalt` startTraceRequest_sampled record   `H.hashWithSalt` startTraceRequest_baggage record   `H.hashWithSalt` startTraceRequest_downstream record  
instance QC.Arbitrary StartTraceRequest where 
  arbitrary = M.liftM StartTraceRequest (QC.arbitrary)
          `M.ap`(QC.arbitrary)
          `M.ap`(QC.arbitrary)
          `M.ap`(QC.arbitrary)
  shrink obj | obj == default_StartTraceRequest = []
             | P.otherwise = M.catMaybes
    [ if obj == default_StartTraceRequest{startTraceRequest_serverRole = startTraceRequest_serverRole obj} then P.Nothing else P.Just $ default_StartTraceRequest{startTraceRequest_serverRole = startTraceRequest_serverRole obj}
    , if obj == default_StartTraceRequest{startTraceRequest_sampled = startTraceRequest_sampled obj} then P.Nothing else P.Just $ default_StartTraceRequest{startTraceRequest_sampled = startTraceRequest_sampled obj}
    , if obj == default_StartTraceRequest{startTraceRequest_baggage = startTraceRequest_baggage obj} then P.Nothing else P.Just $ default_StartTraceRequest{startTraceRequest_baggage = startTraceRequest_baggage obj}
    , if obj == default_StartTraceRequest{startTraceRequest_downstream = startTraceRequest_downstream obj} then P.Nothing else P.Just $ default_StartTraceRequest{startTraceRequest_downstream = startTraceRequest_downstream obj}
    ]
from_StartTraceRequest :: StartTraceRequest -> T.ThriftVal
from_StartTraceRequest record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v13 -> P.Just (1, ("serverRole",T.TString $ E.encodeUtf8 _v13))) $ startTraceRequest_serverRole record
  , (\_v13 -> P.Just (2, ("sampled",T.TBool _v13))) $ startTraceRequest_sampled record
  , (\_v13 -> P.Just (3, ("baggage",T.TString $ E.encodeUtf8 _v13))) $ startTraceRequest_baggage record
  , (\_v13 -> P.Just (4, ("downstream",from_Downstream _v13))) $ startTraceRequest_downstream record
  ]
write_StartTraceRequest :: T.Protocol p => p -> StartTraceRequest -> P.IO ()
write_StartTraceRequest oprot record = T.writeVal oprot $ from_StartTraceRequest record
encode_StartTraceRequest :: T.StatelessProtocol p => p -> StartTraceRequest -> LBS.ByteString
encode_StartTraceRequest oprot record = T.serializeVal oprot $ from_StartTraceRequest record
to_StartTraceRequest :: T.ThriftVal -> StartTraceRequest
to_StartTraceRequest (T.TStruct fields) = StartTraceRequest{
  startTraceRequest_serverRole = P.maybe (P.error "Missing required field: serverRole") (\(_,_val15) -> (case _val15 of {T.TString _val16 -> E.decodeUtf8 _val16; _ -> P.error "wrong type"})) (Map.lookup (1) fields),
  startTraceRequest_sampled = P.maybe (P.error "Missing required field: sampled") (\(_,_val15) -> (case _val15 of {T.TBool _val17 -> _val17; _ -> P.error "wrong type"})) (Map.lookup (2) fields),
  startTraceRequest_baggage = P.maybe (P.error "Missing required field: baggage") (\(_,_val15) -> (case _val15 of {T.TString _val18 -> E.decodeUtf8 _val18; _ -> P.error "wrong type"})) (Map.lookup (3) fields),
  startTraceRequest_downstream = P.maybe (P.error "Missing required field: downstream") (\(_,_val15) -> (case _val15 of {T.TStruct _val19 -> (to_Downstream (T.TStruct _val19)); _ -> P.error "wrong type"})) (Map.lookup (4) fields)
  }
to_StartTraceRequest _ = P.error "not a struct"
read_StartTraceRequest :: T.Protocol p => p -> P.IO StartTraceRequest
read_StartTraceRequest iprot = to_StartTraceRequest <$> T.readVal iprot (T.T_STRUCT typemap_StartTraceRequest)
decode_StartTraceRequest :: T.StatelessProtocol p => p -> LBS.ByteString -> StartTraceRequest
decode_StartTraceRequest iprot bs = to_StartTraceRequest $ T.deserializeVal iprot (T.T_STRUCT typemap_StartTraceRequest) bs
typemap_StartTraceRequest :: T.TypeMap
typemap_StartTraceRequest = Map.fromList [(1,("serverRole",T.T_STRING)),(2,("sampled",T.T_BOOL)),(3,("baggage",T.T_STRING)),(4,("downstream",(T.T_STRUCT typemap_Downstream)))]
default_StartTraceRequest :: StartTraceRequest
default_StartTraceRequest = StartTraceRequest{
  startTraceRequest_serverRole = "",
  startTraceRequest_sampled = P.False,
  startTraceRequest_baggage = "",
  startTraceRequest_downstream = default_Downstream}
data JoinTraceRequest = JoinTraceRequest  { joinTraceRequest_serverRole :: LT.Text
  , joinTraceRequest_downstream :: P.Maybe Downstream
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable JoinTraceRequest where
  hashWithSalt salt record = salt   `H.hashWithSalt` joinTraceRequest_serverRole record   `H.hashWithSalt` joinTraceRequest_downstream record  
instance QC.Arbitrary JoinTraceRequest where 
  arbitrary = M.liftM JoinTraceRequest (QC.arbitrary)
          `M.ap`(M.liftM P.Just QC.arbitrary)
  shrink obj | obj == default_JoinTraceRequest = []
             | P.otherwise = M.catMaybes
    [ if obj == default_JoinTraceRequest{joinTraceRequest_serverRole = joinTraceRequest_serverRole obj} then P.Nothing else P.Just $ default_JoinTraceRequest{joinTraceRequest_serverRole = joinTraceRequest_serverRole obj}
    , if obj == default_JoinTraceRequest{joinTraceRequest_downstream = joinTraceRequest_downstream obj} then P.Nothing else P.Just $ default_JoinTraceRequest{joinTraceRequest_downstream = joinTraceRequest_downstream obj}
    ]
from_JoinTraceRequest :: JoinTraceRequest -> T.ThriftVal
from_JoinTraceRequest record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v22 -> P.Just (1, ("serverRole",T.TString $ E.encodeUtf8 _v22))) $ joinTraceRequest_serverRole record
  , (\_v22 -> (2, ("downstream",from_Downstream _v22))) <$> joinTraceRequest_downstream record
  ]
write_JoinTraceRequest :: T.Protocol p => p -> JoinTraceRequest -> P.IO ()
write_JoinTraceRequest oprot record = T.writeVal oprot $ from_JoinTraceRequest record
encode_JoinTraceRequest :: T.StatelessProtocol p => p -> JoinTraceRequest -> LBS.ByteString
encode_JoinTraceRequest oprot record = T.serializeVal oprot $ from_JoinTraceRequest record
to_JoinTraceRequest :: T.ThriftVal -> JoinTraceRequest
to_JoinTraceRequest (T.TStruct fields) = JoinTraceRequest{
  joinTraceRequest_serverRole = P.maybe (P.error "Missing required field: serverRole") (\(_,_val24) -> (case _val24 of {T.TString _val25 -> E.decodeUtf8 _val25; _ -> P.error "wrong type"})) (Map.lookup (1) fields),
  joinTraceRequest_downstream = P.maybe (P.Nothing) (\(_,_val24) -> P.Just (case _val24 of {T.TStruct _val26 -> (to_Downstream (T.TStruct _val26)); _ -> P.error "wrong type"})) (Map.lookup (2) fields)
  }
to_JoinTraceRequest _ = P.error "not a struct"
read_JoinTraceRequest :: T.Protocol p => p -> P.IO JoinTraceRequest
read_JoinTraceRequest iprot = to_JoinTraceRequest <$> T.readVal iprot (T.T_STRUCT typemap_JoinTraceRequest)
decode_JoinTraceRequest :: T.StatelessProtocol p => p -> LBS.ByteString -> JoinTraceRequest
decode_JoinTraceRequest iprot bs = to_JoinTraceRequest $ T.deserializeVal iprot (T.T_STRUCT typemap_JoinTraceRequest) bs
typemap_JoinTraceRequest :: T.TypeMap
typemap_JoinTraceRequest = Map.fromList [(1,("serverRole",T.T_STRING)),(2,("downstream",(T.T_STRUCT typemap_Downstream)))]
default_JoinTraceRequest :: JoinTraceRequest
default_JoinTraceRequest = JoinTraceRequest{
  joinTraceRequest_serverRole = "",
  joinTraceRequest_downstream = P.Nothing}
data ObservedSpan = ObservedSpan  { observedSpan_traceId :: LT.Text
  , observedSpan_sampled :: P.Bool
  , observedSpan_baggage :: LT.Text
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable ObservedSpan where
  hashWithSalt salt record = salt   `H.hashWithSalt` observedSpan_traceId record   `H.hashWithSalt` observedSpan_sampled record   `H.hashWithSalt` observedSpan_baggage record  
instance QC.Arbitrary ObservedSpan where 
  arbitrary = M.liftM ObservedSpan (QC.arbitrary)
          `M.ap`(QC.arbitrary)
          `M.ap`(QC.arbitrary)
  shrink obj | obj == default_ObservedSpan = []
             | P.otherwise = M.catMaybes
    [ if obj == default_ObservedSpan{observedSpan_traceId = observedSpan_traceId obj} then P.Nothing else P.Just $ default_ObservedSpan{observedSpan_traceId = observedSpan_traceId obj}
    , if obj == default_ObservedSpan{observedSpan_sampled = observedSpan_sampled obj} then P.Nothing else P.Just $ default_ObservedSpan{observedSpan_sampled = observedSpan_sampled obj}
    , if obj == default_ObservedSpan{observedSpan_baggage = observedSpan_baggage obj} then P.Nothing else P.Just $ default_ObservedSpan{observedSpan_baggage = observedSpan_baggage obj}
    ]
from_ObservedSpan :: ObservedSpan -> T.ThriftVal
from_ObservedSpan record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v29 -> P.Just (1, ("traceId",T.TString $ E.encodeUtf8 _v29))) $ observedSpan_traceId record
  , (\_v29 -> P.Just (2, ("sampled",T.TBool _v29))) $ observedSpan_sampled record
  , (\_v29 -> P.Just (3, ("baggage",T.TString $ E.encodeUtf8 _v29))) $ observedSpan_baggage record
  ]
write_ObservedSpan :: T.Protocol p => p -> ObservedSpan -> P.IO ()
write_ObservedSpan oprot record = T.writeVal oprot $ from_ObservedSpan record
encode_ObservedSpan :: T.StatelessProtocol p => p -> ObservedSpan -> LBS.ByteString
encode_ObservedSpan oprot record = T.serializeVal oprot $ from_ObservedSpan record
to_ObservedSpan :: T.ThriftVal -> ObservedSpan
to_ObservedSpan (T.TStruct fields) = ObservedSpan{
  observedSpan_traceId = P.maybe (P.error "Missing required field: traceId") (\(_,_val31) -> (case _val31 of {T.TString _val32 -> E.decodeUtf8 _val32; _ -> P.error "wrong type"})) (Map.lookup (1) fields),
  observedSpan_sampled = P.maybe (P.error "Missing required field: sampled") (\(_,_val31) -> (case _val31 of {T.TBool _val33 -> _val33; _ -> P.error "wrong type"})) (Map.lookup (2) fields),
  observedSpan_baggage = P.maybe (P.error "Missing required field: baggage") (\(_,_val31) -> (case _val31 of {T.TString _val34 -> E.decodeUtf8 _val34; _ -> P.error "wrong type"})) (Map.lookup (3) fields)
  }
to_ObservedSpan _ = P.error "not a struct"
read_ObservedSpan :: T.Protocol p => p -> P.IO ObservedSpan
read_ObservedSpan iprot = to_ObservedSpan <$> T.readVal iprot (T.T_STRUCT typemap_ObservedSpan)
decode_ObservedSpan :: T.StatelessProtocol p => p -> LBS.ByteString -> ObservedSpan
decode_ObservedSpan iprot bs = to_ObservedSpan $ T.deserializeVal iprot (T.T_STRUCT typemap_ObservedSpan) bs
typemap_ObservedSpan :: T.TypeMap
typemap_ObservedSpan = Map.fromList [(1,("traceId",T.T_STRING)),(2,("sampled",T.T_BOOL)),(3,("baggage",T.T_STRING))]
default_ObservedSpan :: ObservedSpan
default_ObservedSpan = ObservedSpan{
  observedSpan_traceId = "",
  observedSpan_sampled = P.False,
  observedSpan_baggage = ""}
data TraceResponse = TraceResponse  { traceResponse_span :: P.Maybe ObservedSpan
  , traceResponse_downstream :: P.Maybe TraceResponse
  , traceResponse_notImplementedError :: LT.Text
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable TraceResponse where
  hashWithSalt salt record = salt   `H.hashWithSalt` traceResponse_span record   `H.hashWithSalt` traceResponse_downstream record   `H.hashWithSalt` traceResponse_notImplementedError record  
instance QC.Arbitrary TraceResponse where 
  arbitrary = M.liftM TraceResponse (M.liftM P.Just QC.arbitrary)
          `M.ap`(M.liftM P.Just QC.arbitrary)
          `M.ap`(QC.arbitrary)
  shrink obj | obj == default_TraceResponse = []
             | P.otherwise = M.catMaybes
    [ if obj == default_TraceResponse{traceResponse_span = traceResponse_span obj} then P.Nothing else P.Just $ default_TraceResponse{traceResponse_span = traceResponse_span obj}
    , if obj == default_TraceResponse{traceResponse_downstream = traceResponse_downstream obj} then P.Nothing else P.Just $ default_TraceResponse{traceResponse_downstream = traceResponse_downstream obj}
    , if obj == default_TraceResponse{traceResponse_notImplementedError = traceResponse_notImplementedError obj} then P.Nothing else P.Just $ default_TraceResponse{traceResponse_notImplementedError = traceResponse_notImplementedError obj}
    ]
from_TraceResponse :: TraceResponse -> T.ThriftVal
from_TraceResponse record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v37 -> (1, ("span",from_ObservedSpan _v37))) <$> traceResponse_span record
  , (\_v37 -> (2, ("downstream",from_TraceResponse _v37))) <$> traceResponse_downstream record
  , (\_v37 -> P.Just (3, ("notImplementedError",T.TString $ E.encodeUtf8 _v37))) $ traceResponse_notImplementedError record
  ]
write_TraceResponse :: T.Protocol p => p -> TraceResponse -> P.IO ()
write_TraceResponse oprot record = T.writeVal oprot $ from_TraceResponse record
encode_TraceResponse :: T.StatelessProtocol p => p -> TraceResponse -> LBS.ByteString
encode_TraceResponse oprot record = T.serializeVal oprot $ from_TraceResponse record
to_TraceResponse :: T.ThriftVal -> TraceResponse
to_TraceResponse (T.TStruct fields) = TraceResponse{
  traceResponse_span = P.maybe (P.Nothing) (\(_,_val39) -> P.Just (case _val39 of {T.TStruct _val40 -> (to_ObservedSpan (T.TStruct _val40)); _ -> P.error "wrong type"})) (Map.lookup (1) fields),
  traceResponse_downstream = P.maybe (P.Nothing) (\(_,_val39) -> P.Just (case _val39 of {T.TStruct _val41 -> (to_TraceResponse (T.TStruct _val41)); _ -> P.error "wrong type"})) (Map.lookup (2) fields),
  traceResponse_notImplementedError = P.maybe (P.error "Missing required field: notImplementedError") (\(_,_val39) -> (case _val39 of {T.TString _val42 -> E.decodeUtf8 _val42; _ -> P.error "wrong type"})) (Map.lookup (3) fields)
  }
to_TraceResponse _ = P.error "not a struct"
read_TraceResponse :: T.Protocol p => p -> P.IO TraceResponse
read_TraceResponse iprot = to_TraceResponse <$> T.readVal iprot (T.T_STRUCT typemap_TraceResponse)
decode_TraceResponse :: T.StatelessProtocol p => p -> LBS.ByteString -> TraceResponse
decode_TraceResponse iprot bs = to_TraceResponse $ T.deserializeVal iprot (T.T_STRUCT typemap_TraceResponse) bs
typemap_TraceResponse :: T.TypeMap
typemap_TraceResponse = Map.fromList [(1,("span",(T.T_STRUCT typemap_ObservedSpan))),(2,("downstream",(T.T_STRUCT typemap_TraceResponse))),(3,("notImplementedError",T.T_STRING))]
default_TraceResponse :: TraceResponse
default_TraceResponse = TraceResponse{
  traceResponse_span = P.Nothing,
  traceResponse_downstream = P.Nothing,
  traceResponse_notImplementedError = ""}

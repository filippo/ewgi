%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @author Davide Marquês <nesrait@gmail.com>
%% @copyright 2009 Smarkets Limited.
%%
%% @doc Secure data transformations for data storage in unsecure locations.
%% Based on smak_sn_cookie by Hunter Morris.
%% Differences from smak_sn_cookie: removed the timestamping operation on
%% data encoding and and the timeout validation on decoding data.
%%
%% The binary data is first compressed, then encrypted using AES in CFB mode
%% with a random initialization vector. After that the data is signed
%% using HMAC-SHA1 with the same key.
%%
%% When the data is decoded, the signature is first checked for validity and
%% then the data is decrypted, decompressed, and deserialized.
%%
%% @end
%%
%% Licensed under the MIT license:
%% http://www.opensource.org/licenses/mit-license.php

-module(ewgi_util_crypto).
-author('Hunter Morris <hunter.morris@smarkets.com>').
-author('Davide Marquês <nesrait@gmail.com>').

-define(MAX_SIZE, 4096). %% Most browsers only allow 4K of cookies
-define(MAX_SIZE_REPR, ?MAX_SIZE/2). %% Assume a 2-character representation of each byte in the final cookie.

-export([encode/2, encode/3, decode/2]).

%% @spec encode(Key::binary(), Data::binary()) -> binary() | {error, Reason::atom()}
%% @doc Encodes and signs the data using the specified secret key.
%% @see encode/3
-spec encode(binary(), binary()) -> binary() | {error, atom()}.
encode(Key, Data) ->
    encode(Key, Data, ?MAX_SIZE_REPR).

%% @spec encode(Key::binary(), Data::binary(), MaxSize::integer()) -> binary() | {error, Reason::atom()}
%% @doc Encodes and signs the data using the specified secret key and provides an error if it exceeds the maximum size.
-spec encode(binary(), binary(), integer()) -> binary() | {error, atom()}.
encode(<<Key:16/binary>>, Data, MaxSize) when MaxSize > 0, is_binary(Data) ->
    IV = crypto:rand_bytes(16),
    DataCompressed = zlib:zip(Data),
    DataCrypted = crypto:aes_cfb_128_encrypt(Key, IV, DataCompressed),
    HMACSignature = crypto:sha_mac(Key, <<IV:16/binary, DataCrypted/bits>>),
    Result = <<HMACSignature:20/binary, IV:16/binary, DataCrypted/bits>>,
    case size(Result) of
        Sz when Sz >= MaxSize ->
            {error, session_too_large};
        _ ->
            Result
    end;
encode(_Key, _, _) ->
    {error, invalid_key}.
    
%% @spec decode(Key::binary(), Cookie::binary()) -> binary() | {error, Reason::atom()}
%% @doc Checks the signature and timeout and decrypts the cookie if it is valid.
-spec decode(binary(), binary()) -> binary() | {error, atom()}.
decode(<<Key:16/binary>>, <<HMACSignature:20/binary, IV:16/binary, DataCrypted/bits>>) ->
    case crypto:sha_mac(Key, <<IV:16/binary, DataCrypted/bits>>) of
        HMACSignature ->
	    DataCompressed = crypto:aes_cfb_128_decrypt(Key, IV, DataCrypted),
	    Data = zlib:unzip(DataCompressed),
	    Data;
        _ ->
            {error, data_tampered}
    end;
decode(_Key, _) ->
    {error, invalid_key}.

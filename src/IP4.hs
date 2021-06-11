module IP4
  ( IP4, ip4 )
where

import Prelude  ( seq )

-- aeson -------------------------------

import Data.Aeson.Types  ( typeMismatch )

-- base --------------------------------

import Control.Applicative  ( pure )
import Control.Monad        ( fail, return )
import Data.Eq              ( Eq )
import Data.Function        ( ($), (&) )
import Data.Functor         ( fmap )
import Data.Ord             ( Ord )
import Data.Void            ( Void )
import Text.Show            ( Show )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- data-default ------------------------

import Data.Default  ( def )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed, Malformed ), Printable( print )
                     , parseText, toText )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData( rnf ) )

-- dhall -------------------------------

import qualified  Dhall.Core  as  DC

import Data.Either.Validation  ( Validation( Success ) )
import Dhall       ( Decoder( Decoder, expected, extract ), Expector, Extractor
                   , FromDhall( autoWith ), Generic, typeError )
import Dhall.Core  ( Expr )
import Dhall.Src   ( Src )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⋪), (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Lens         ( (⊩) )
import Data.MoreUnicode.Maybe        ( pattern 𝕵 )

-- network-ip --------------------------

import qualified  Network.IP.Addr  as  IPAddr

-- parsec ------------------------------

import Text.Parsec.Char  ( char )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ), __parsecN__ )

-- quasiquoting ------------------------

import QuasiQuoting  ( exp,mkQQ )

-- template-haskell --------------------

import Language.Haskell.TH        ( appE, litE, stringL, varE )
import Language.Haskell.TH.Quote  ( QuasiQuoter )

-- text --------------------------------

import Data.Text  ( Text, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- yaml --------------------------------

import Data.Yaml  ( FromJSON( parseJSON ), Value( String ) )

--------------------------------------------------------------------------------

newtype IP4 = IP4 IPAddr.IP4
  deriving (Eq, Generic, Ord, Show)

instance FromJSON IP4 where
  parseJSON (String t) = case parseText t of
                           Parsed    i4  → return $ IP4 i4
                           Malformed _ e → fail (e ⊕ " (" ⊕ unpack t ⊕ ")")
  parseJSON invalid    = typeMismatch "IP4" invalid

instance Printable IP4 where
  print (IP4 ipv4) = P.text (toText ipv4)

instance NFData IP4 where
  rnf i = seq i ()

------------------------------------------------------------

instance Parsecable IP4 where
  parser = fmap IP4 $ IPAddr.ip4FromOctets ⊳ parser ⋪ char '.'
                                           ⊵ parser ⋪ char '.'
                                           ⊵ parser ⋪ char '.'
                                           ⊵ parser


__ip4 ∷ Text → IP4
__ip4 = __parsecN__

{-| quasi-quoter for ipv4 addresses -}
ip4 ∷ QuasiQuoter
ip4 =
  let mkExp t = 𝕵 $ appE (varE '__ip4) (litE $ stringL t)
   in mkQQ "IP4" $ def & exp ⊩ mkExp

instance FromDhall IP4 where
  autoWith _ = Decoder {..}
               where expected ∷ Expector (Expr Src Void)
                     expected = Success DC.Text
                     extract ∷ Expr Src Void → Extractor Src Void IP4
                     extract (DC.TextLit (DC.Chunks [] t)) = pure $ __parsecN__ t
                     extract x                             = typeError expected x

-- that's all, folks! ----------------------------------------------------------

module Arkham.Homebrew.AgesUnwound.Treacheries.BeckoningOfEverywhen (beckoningOfEverywhen) where

import Arkham.ChaosToken
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Homebrew.AgesUnwound.Traits (pattern Adrift)
import Arkham.Matcher
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Treachery.Import.Lifted

newtype BeckoningOfEverywhen = BeckoningOfEverywhen TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beckoningOfEverywhen :: TreacheryCard BeckoningOfEverywhen
beckoningOfEverywhen = treachery BeckoningOfEverywhen Cards.beckoningOfEverywhen

{- | "Revelation - If you are not at an [[Adrift]] location, Beckoning of
Everywhen gains surge. Otherwise, reveal a random token from the chaos bag. If
you revealed a non-zero number, swap the positions of your location and the
location X positions clockwise, where X is the modifier of the revealed token.
If you revealed a [skull], [cultist], [tablet], [elder_thing], [curse] or
[auto_fail], move to the location across from you."

X is the token's /printed/ modifier, so a @-2@ is minus two positions clockwise
-- two counter-clockwise -- which the ring's modular arithmetic handles. The
second clause names every symbol in this campaign's bag plus [curse]; a [bless]
(which no Ages Unwound effect adds, though a blessed player deck can) is
deliberately not on the list, so it does nothing.
-}

-- | The symbols the card's second clause names, verbatim.
movingSymbols :: [ChaosTokenFace]
movingSymbols = [Skull, Cultist, Tablet, ElderThing, CurseToken, AutoFail]

instance RunMessage BeckoningOfEverywhen where
  runMessage msg t@(BeckoningOfEverywhen attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      atAdrift <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Adrift
      if atAdrift
        then requestChaosTokens iid attrs 1
        else gainSurge attrs
      pure t
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      for_ tokens \token -> do
        let face = token.face
        mlid <- selectOne $ locationWithInvestigator iid
        for_ mlid \lid ->
          if isNumberChaosToken face
            then do
              let x = chaosTokenToFaceValue face
              when (x /= 0) do
                getClockwise (x `mod` ringSize) lid >>= traverse_ (swapRingPositions lid)
            else
              when (face `elem` movingSymbols)
                $ getAcross lid
                >>= traverse_ (moveTo attrs iid)
      resetChaosTokens attrs
      pure t
    _ -> BeckoningOfEverywhen <$> liftRunMessage msg attrs

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
      continue_ iid
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

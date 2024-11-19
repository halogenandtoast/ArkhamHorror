module Arkham.Location.Cards.IcyWastes (icyWastes, IcyWastes (..)) where

import Arkham.ChaosToken
import Arkham.Cost
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens, isSkillTestAt, withSkillTest)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (RevealChaosToken)

newtype IcyWastes = IcyWastes LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

icyWastes :: LocationCard IcyWastes
icyWastes =
  symbolLabel
    $ locationWith IcyWastes Cards.icyWastes 2 (PerPlayer 2)
    $ (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance RunMessage IcyWastes where
  runMessage msg l@(IcyWastes attrs) = runQueueT $ case msg of
    RevealChaosToken _ _ token | token.face == #frost -> do
      whenM (isSkillTestAt attrs) do
        withSkillTest \sid -> do
          tokens <- getSkillTestRevealedChaosTokens
          -- At this point the token will not be in the revealed tokens so we
          -- check if it is not, and also if the current frost token count is 0
          when (token `notElem` tokens && count ((== #frost) . (.face)) tokens == 0) do
            skillTestModifier sid attrs token (ChangeChaosTokenModifier $ NegativeModifier 3)
      pure l
    _ -> IcyWastes <$> liftRunMessage msg attrs

module Arkham.Location.Cards.SubnauticalSprawl (subnauticalSprawl) where

import Arkham.ChaosToken
import Arkham.Helpers.Modifiers (ModifierType (..), semaphore)
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens, isSkillTestAt, withSkillTest)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SubnauticalSprawl = SubnauticalSprawl LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

subnauticalSprawl :: LocationCard SubnauticalSprawl
subnauticalSprawl = location SubnauticalSprawl Cards.subnauticalSprawl 2 (PerPlayer 2)

instance RunMessage SubnauticalSprawl where
  runMessage msg l@(SubnauticalSprawl attrs) = runQueueT $ case msg of
    RevealChaosToken _ _ token | token.face == #frost -> do
      semaphore attrs do
        whenM (isSkillTestAt attrs) do
          withSkillTest \sid -> do
            tokens <- getSkillTestRevealedChaosTokens
            -- At this point the token will not be in the revealed tokens so we
            -- check if it is not, and also if the current frost token count is 0
            when (token `notElem` tokens && count ((== #frost) . (.face)) tokens == 0) do
              roundModifier attrs attrs Semaphore
              skillTestModifier sid attrs token (ChangeChaosTokenModifier $ NegativeModifier 3)
      pure l
    _ -> SubnauticalSprawl <$> liftRunMessage msg attrs

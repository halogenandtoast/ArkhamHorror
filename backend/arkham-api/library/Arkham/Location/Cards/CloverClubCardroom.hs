module Arkham.Location.Cards.CloverClubCardroom (cloverClubCardroom) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.GameValue
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFaces)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards (cloverClubCardroom)
import Arkham.Location.Import.Lifted

newtype CloverClubCardroom = CloverClubCardroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubCardroom :: LocationCard CloverClubCardroom
cloverClubCardroom = symbolLabel $ location CloverClubCardroom Cards.cloverClubCardroom 3 (Static 0)

instance HasAbilities CloverClubCardroom where
  getAbilities (CloverClubCardroom a) =
    extendRevealed1 a
      $ restricted a 1 (OnAct 1 <> Here)
      $ actionAbilityWithCost (ResourceCost 2)

instance RunMessage CloverClubCardroom where
  runMessage msg l@(CloverClubCardroom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      requestChaosTokens iid (attrs.ability 1) 1
      pure l
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      chaosTokenFaces <- getModifiedChaosTokenFaces tokens
      withI18n $ prompt_ iid "applyResults"
      for_ chaosTokenFaces \case
        ElderSign -> do
          gainClues iid (attrs.ability 1) 2
          gainResources iid (attrs.ability 1) 2
        PlusOne -> pure ()
        Zero -> gainClues iid (attrs.ability 1) 2
        MinusOne -> pure ()
        MinusTwo -> gainClues iid (attrs.ability 1) 2
        MinusThree -> pure ()
        MinusFour -> gainClues iid (attrs.ability 1) 2
        MinusFive -> pure ()
        MinusSix -> gainClues iid (attrs.ability 1) 2
        MinusSeven -> pure ()
        MinusEight -> gainClues iid (attrs.ability 1) 2
        Skull -> pure ()
        Cultist -> pure ()
        Tablet -> pure ()
        ElderThing -> pure ()
        AutoFail -> pure ()
        CurseToken -> pure ()
        BlessToken -> pure ()
        FrostToken -> pure ()
      resetChaosTokens (attrs.ability 1)
      pure l
    _ -> CloverClubCardroom <$> liftRunMessage msg attrs

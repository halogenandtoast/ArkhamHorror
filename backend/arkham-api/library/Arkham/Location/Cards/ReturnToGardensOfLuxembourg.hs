module Arkham.Location.Cards.ReturnToGardensOfLuxembourg (returnToGardensOfLuxembourg) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Byakhee))

newtype ReturnToGardensOfLuxembourg = ReturnToGardensOfLuxembourg LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToGardensOfLuxembourg :: LocationCard ReturnToGardensOfLuxembourg
returnToGardensOfLuxembourg = location ReturnToGardensOfLuxembourg Cards.returnToGardensOfLuxembourg 2 (PerPlayer 1)

instance HasAbilities ReturnToGardensOfLuxembourg where
  getAbilities (ReturnToGardensOfLuxembourg a) =
    extendRevealed1 a
      $ restricted a 1 (exists $ EnemyWithTrait Byakhee <> oneOf [not_ (at_ $ be a), ExhaustedEnemy])
      $ forced
      $ Enters #after You (be a)

instance RunMessage ReturnToGardensOfLuxembourg where
  runMessage msg l@(ReturnToGardensOfLuxembourg attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      requestChaosTokens iid (attrs.ability 1) 1
      pure l
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) (map (.face) -> tokens) -> do
      withI18n $ prompt_ iid "continue"
      let triggeringTokens = [#skull, #cultist, #tablet, #elderthing, #autofail]
      when (any (`elem` triggeringTokens) tokens) do
        byakhee <- select $ EnemyWithTrait Byakhee <> oneOf [not_ (at_ $ be attrs), ExhaustedEnemy]
        chooseOneAtATimeM iid do
          targets byakhee \x -> do
            readyThis x
            enemyMoveTo x attrs
      pure l
    _ -> ReturnToGardensOfLuxembourg <$> liftRunMessage msg attrs

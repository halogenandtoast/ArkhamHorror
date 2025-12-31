module Arkham.Act.Cards.TheCarnevaleConspiracy (theCarnevaleConspiracy) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype TheCarnevaleConspiracy = TheCarnevaleConspiracy ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCarnevaleConspiracy :: ActCard TheCarnevaleConspiracy
theCarnevaleConspiracy = act (1, A) TheCarnevaleConspiracy Cards.theCarnevaleConspiracy Nothing

instance HasAbilities TheCarnevaleConspiracy where
  getAbilities = actAbilities \x ->
    [ restricted
        x
        1
        (exists $ "Masked Carnevale-Goer" <> AssetWithoutModifier CannotBeRevealed)
        (actionAbilityWithCost $ GroupClueCost (PerPlayer 1) Anywhere)
    , restricted
        x
        2
        ( UnderneathCardCount (EqualTo $ Static 3) (UnderActDeck <> UnderAgendaDeck)
            $ oneOf [cardIs Assets.innocentReveler, CardWithTitle "Masked Carnevale-Goer"]
        )
        $ forced AnyWindow
    ]

instance RunMessage TheCarnevaleConspiracy where
  runMessage msg a@(TheCarnevaleConspiracy attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      maskedCarnevaleGoers <- select $ "Masked Carnevale-Goer" <> AssetWithoutModifier CannotBeRevealed
      chooseTargetM iid maskedCarnevaleGoers (lookAtRevealed iid (attrs.ability 1))
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      createEnemyCard_ Enemies.cnidathqua Global
      advanceActDeck attrs
      lead <- getLead
      maskedCarnevaleGoers <- select (AssetWithTitle "Masked Carnevale-Goer")
      chooseTargetM lead maskedCarnevaleGoers (flipOverBy lead (attrs.ability 2))
      pure a
    _ -> TheCarnevaleConspiracy <$> liftRunMessage msg attrs

module Arkham.Asset.Assets.JeremiahKirbyArcticArchaeologist (jeremiahKirbyArcticArchaeologist) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Search
import Arkham.Strategy
import Arkham.Taboo

newtype JeremiahKirbyArcticArchaeologist = JeremiahKirbyArcticArchaeologist AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeremiahKirbyArcticArchaeologist :: AssetCard JeremiahKirbyArcticArchaeologist
jeremiahKirbyArcticArchaeologist = ally JeremiahKirbyArcticArchaeologist Cards.jeremiahKirbyArcticArchaeologist (2, 1)

instance HasModifiersFor JeremiahKirbyArcticArchaeologist where
  getModifiersFor (JeremiahKirbyArcticArchaeologist a) = controllerGets a [SkillModifier #intellect 1]

instance HasAbilities JeremiahKirbyArcticArchaeologist where
  getAbilities (JeremiahKirbyArcticArchaeologist a) =
    [applyTaboo $ restricted a 1 ControlsThis $ freeReaction $ AssetEntersPlay #when (be a)]
   where
    applyTaboo =
      if tabooed TabooList21 a
        then limited (MaxPer Cards.jeremiahKirbyArcticArchaeologist PerGame 2)
        else id

instance RunMessage JeremiahKirbyArcticArchaeologist where
  runMessage msg a@(JeremiahKirbyArcticArchaeologist attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- technically the choose is part of the cost, but I don't think we care
      let source = attrs.ability 1
      let
        revealTopOfDeck mtch =
          revealingEdit iid source iid (FromTopOfDeck 5) \s ->
            s
              { searchZones = [fromTopOfDeck 5]
              , searchMatcher = basic mtch
              , searchFoundStrategy = DrawAllFound iid
              }
      chooseOneM iid $ withI18n do
        labeledI18n "even" $ revealTopOfDeck CardWithEvenCost
        labeledI18n "odd" $ revealTopOfDeck CardWithOddCost
      pure a
    _ -> JeremiahKirbyArcticArchaeologist <$> liftRunMessage msg attrs

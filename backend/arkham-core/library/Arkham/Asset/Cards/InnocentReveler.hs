module Arkham.Asset.Cards.InnocentReveler (innocentReveler, InnocentReveler (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Prelude

newtype InnocentReveler = InnocentReveler AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innocentReveler :: AssetCard InnocentReveler
innocentReveler =
  allyWith InnocentReveler Cards.innocentReveler (2, 2) $ (slotsL .~ mempty) . (isStoryL .~ True)

instance HasAbilities InnocentReveler where
  getAbilities (InnocentReveler x) =
    [ restrictedAbility x 1 (Uncontrolled <> OnSameLocation) parleyAction_
    , mkAbility x 2 $ forced $ AssetWouldBeDiscarded #when (be x)
    ]

instance RunMessage InnocentReveler where
  runMessage msg a@(InnocentReveler attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      push $ parley sid iid (attrs.ability 1) (toTarget attrs) #intellect (Fixed 2)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      investigatorIds <- getInvestigatorIds
      let card = PlayerCard $ lookupPlayerCard (toCardDef attrs) (toCardId attrs)
      pushAll
        $ PlaceUnderneath AgendaDeckTarget [card]
        : [ assignHorror iid' (attrs.ability 2) 1
          | iid' <- investigatorIds
          ]
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ TakeControlOfAsset iid attrs.id
      pure a
    _ -> InnocentReveler <$> runMessage msg attrs

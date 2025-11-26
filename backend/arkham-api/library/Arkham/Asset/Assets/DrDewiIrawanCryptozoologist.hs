module Arkham.Asset.Assets.DrDewiIrawanCryptozoologist (drDewiIrawanCryptozoologist) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Data.List.Extra (groupSort)

newtype DrDewiIrawanCryptozoologist = DrDewiIrawanCryptozoologist AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drDewiIrawanCryptozoologist :: AssetCard DrDewiIrawanCryptozoologist
drDewiIrawanCryptozoologist = asset DrDewiIrawanCryptozoologist Cards.drDewiIrawanCryptozoologist

instance HasModifiersFor DrDewiIrawanCryptozoologist where
  getModifiersFor (DrDewiIrawanCryptozoologist a) = controllerGets a [SkillModifier #willpower 1]

instance HasAbilities DrDewiIrawanCryptozoologist where
  getAbilities (DrDewiIrawanCryptozoologist a) =
    [ controlled
        a
        1
        ( oneOf
            [ exists (InHandOf NotForPlay You <> CardWithHollowedCopy)
            , exists (InHandOf NotForPlay You <> basic DiscardableCard) <> exists HollowedCard
            ]
        )
        $ FastAbility (exhaust a)
    ]

instance RunMessage DrDewiIrawanCryptozoologist where
  runMessage msg a@(DrDewiIrawanCryptozoologist attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canReveal <- selectAny $ inHandOf NotForPlay iid <> CardWithHollowedCopy
      canDiscard <- selectAny $ inHandOf NotForPlay iid <> basic DiscardableCard
      chooseOneM iid $ campaignI18n do
        labeledValidate' canReveal "drDewiIrawan.reveal" $ doStep 1 msg
        labeledValidate' canDiscard "drDewiIrawan.discard" $ doStep 2 msg
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      cards <- select $ inHandOf NotForPlay iid <> CardWithHollowedCopy
      chooseTargetM iid cards \card -> do
        revealCard card
        hollows <- select $ HollowedCard <> basic (cardIs card)
        let withOwners = hollows & mapMaybe (\c -> (,c) <$> c.owner) & groupSort
        for_ withOwners (uncurry shuffleCardsIntoDeck)
      pure a
    DoStep 2 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      chooseAndDiscardCard iid (attrs.ability 1)
      hollows <- select HollowedCard
      let withOwners = hollows & mapMaybe (\c -> (,c) <$> c.owner)
      chooseOneM iid $ for_ withOwners \(owner, c) -> targeting c $ shuffleCardsIntoDeck owner [c]
      pure a
    _ -> DrDewiIrawanCryptozoologist <$> liftRunMessage msg attrs

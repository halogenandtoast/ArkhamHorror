module Arkham.Asset.Cards.GuidedByTheUnseen3 (guidedByTheUnseen3, GuidedByTheUnseen3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Deck qualified as Deck
import Arkham.Helpers.SkillTest (getIsCommittable, getSkillTestInvestigator)
import Arkham.Matcher
import Arkham.Strategy

newtype GuidedByTheUnseen3 = GuidedByTheUnseen3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guidedByTheUnseen3 :: AssetCard GuidedByTheUnseen3
guidedByTheUnseen3 = asset GuidedByTheUnseen3 Cards.guidedByTheUnseen3

instance HasAbilities GuidedByTheUnseen3 where
  getAbilities (GuidedByTheUnseen3 x) =
    [ playerLimit PerTestOrAbility
        $ controlledAbility
          x
          1
          ( DuringSkillTest
              $ SkillTestAtYourLocation
              <> SkillTestOfInvestigator (oneOf [can.search.deck, can.manipulate.deck])
          )
        $ FastAbility Free
    ]

instance RunMessage GuidedByTheUnseen3 where
  runMessage msg a@(GuidedByTheUnseen3 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      iid <- fromJustNote "not skill test" <$> getSkillTestInvestigator
      canSearchDeck <- can.search.deck iid
      if canSearchDeck
        then
          search iid (attrs.ability 1) iid [fromTopOfDeck 3] AnyCard (DeferSearchedToTarget $ toTarget attrs)
        else whenM (can.shuffle.deck iid) $ push $ ShuffleDeck $ Deck.InvestigatorDeck iid
      pure a
    SearchFound _iid (isTarget attrs -> True) _ cards | notNull cards -> do
      iid <- fromJustNote "not skill test" <$> getSkillTestInvestigator
      committable <- filterM (getIsCommittable iid) cards
      focusCards cards \unfocus -> do
        if attrs.use Secret == 0 || null committable
          then chooseOne iid [Label "Continue" [unfocus]]
          else
            chooseOne iid $ Label "Do not commit any cards" [unfocus]
              : [ targetLabel card [SpendUses (toTarget attrs) Secret 1, SkillTestCommitCard iid card]
                | card <- committable
                ]
      pure a
    _ -> GuidedByTheUnseen3 <$> lift (runMessage msg attrs)

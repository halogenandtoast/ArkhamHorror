module Arkham.Asset.Cards.InnocentReveler
  ( innocentReveler
  , InnocentReveler(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype InnocentReveler = InnocentReveler AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innocentReveler :: AssetCard InnocentReveler
innocentReveler = allyWith
  InnocentReveler
  Cards.innocentReveler
  (2, 2)
  ((slotsL .~ mempty) . (isStoryL .~ True))

instance HasAbilities InnocentReveler where
  getAbilities (InnocentReveler x) =
    [ restrictedAbility x 1 (Uncontrolled <> OnSameLocation)
      $ ActionAbility (Just Parley) (ActionCost 1)
    , mkAbility x 2
      $ ForcedAbility
      $ AssetWouldBeDiscarded Timing.When
      $ AssetWithId
      $ toId x
    ]

instance RunMessage InnocentReveler where
  runMessage msg a@(InnocentReveler attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      a
        <$ push
             (BeginSkillTest
               iid
               source
               (toTarget attrs)
               (Just Parley)
               SkillIntellect
               2
             )
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      investigatorIds <- getInvestigatorIds
      let
        card = PlayerCard $ lookupPlayerCard (toCardDef attrs) (toCardId attrs)
      a <$ pushAll
        (PlaceUnderneath AgendaDeckTarget [card]
        : [ InvestigatorAssignDamage iid' (toSource attrs) DamageAny 0 1
          | iid' <- investigatorIds
          ]
        )
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> a <$ push (TakeControlOfAsset iid $ toId attrs)
    _ -> InnocentReveler <$> runMessage msg attrs


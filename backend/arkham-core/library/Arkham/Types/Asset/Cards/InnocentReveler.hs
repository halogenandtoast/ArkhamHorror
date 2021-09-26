module Arkham.Types.Asset.Cards.InnocentReveler
  ( innocentReveler
  , InnocentReveler(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Card
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype InnocentReveler = InnocentReveler AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innocentReveler :: AssetCard InnocentReveler
innocentReveler = allyWith
  InnocentReveler
  Cards.innocentReveler
  (2, 2)
  ((slotsL .~ mempty) . (isStoryL .~ True))

instance HasAbilities InnocentReveler where
  getAbilities (InnocentReveler x) =
    [ restrictedAbility x 1 (Unowned <> OnSameLocation)
      $ ActionAbility (Just Parley) (ActionCost 1)
    , mkAbility x 2
      $ ForcedAbility
      $ AssetWouldBeDiscarded Timing.When
      $ AssetWithId
      $ toId x
    ]

instance
  ( HasSet InvestigatorId env ()
  , HasQueue env
  , HasModifiersFor env ()
  )
  => RunMessage env InnocentReveler where
  runMessage msg a@(InnocentReveler attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
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
    UseCardAbility _ source _ 2 _ | isSource attrs source -> do
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


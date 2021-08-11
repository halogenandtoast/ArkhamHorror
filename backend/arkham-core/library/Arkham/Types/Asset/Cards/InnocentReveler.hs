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
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype InnocentReveler = InnocentReveler AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innocentReveler :: AssetCard InnocentReveler
innocentReveler = ally InnocentReveler Cards.innocentReveler (2, 2)

instance HasActions InnocentReveler where
  getActions (InnocentReveler x) =
    [ restrictedAbility x 1 (Unowned <> OnSameLocation)
        $ ActionAbility (Just Parley) (ActionCost 1)
    ]

instance HasModifiersFor env InnocentReveler

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
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> a <$ push (TakeControlOfAsset iid $ toId attrs)
    When (Discard target) | isTarget attrs target -> do
      investigatorIds <- getInvestigatorIds
      let
        card = PlayerCard $ lookupPlayerCard (toCardDef attrs) (toCardId attrs)
      a <$ pushAll
        (PlaceUnderneath AgendaDeckTarget [card]
        : [ InvestigatorAssignDamage iid' (toSource attrs) DamageAny 0 1
          | iid' <- investigatorIds
          ]
        )
    _ -> InnocentReveler <$> runMessage msg attrs


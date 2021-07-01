module Arkham.Types.Asset.Cards.WhittonGreene
  ( whittonGreene
  , WhittonGreene(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window

newtype WhittonGreene = WhittonGreene AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whittonGreene :: AssetCard WhittonGreene
whittonGreene = ally WhittonGreene Cards.whittonGreene (2, 2)

instance HasActions env WhittonGreene where
  getActions iid (AfterRevealLocation You) (WhittonGreene a) | ownedBy a iid =
    do
      let
        ability =
          mkAbility (toSource a) 1 (ReactionAbility $ ExhaustCost (toTarget a))
      pure [ActivateCardAbilityAction iid ability]
  getActions iid (AfterPutLocationIntoPlay You) (WhittonGreene a)
    | ownedBy a iid = do
      let
        ability =
          mkAbility (toSource a) 1 (ReactionAbility $ ExhaustCost (toTarget a))
      pure [ActivateCardAbilityAction iid ability]
  getActions iid window (WhittonGreene attrs) = getActions iid window attrs

instance HasCount AssetCount env (InvestigatorId, [Trait]) => HasModifiersFor env WhittonGreene where
  getModifiersFor _ (InvestigatorTarget iid) (WhittonGreene a) | ownedBy a iid =
    do
      active <- (> 0) . unAssetCount <$> getCount (iid, [Tome, Relic])
      pure $ toModifiers a [ SkillModifier SkillIntellect 1 | active ]
  getModifiersFor _ _ _ = pure []

instance AssetRunner env => RunMessage env WhittonGreene where
  runMessage msg a@(WhittonGreene attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage
        (SearchTopOfDeck
          iid
          (InvestigatorTarget iid)
          6
          [Tome, Relic]
          ShuffleBackIn
        )
    _ -> WhittonGreene <$> runMessage msg attrs

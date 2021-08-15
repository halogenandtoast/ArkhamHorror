module Arkham.Types.Asset.Cards.Scavenging
  ( Scavenging(..)
  , scavenging
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window

newtype Scavenging = Scavenging AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scavenging :: AssetCard Scavenging
scavenging = asset Scavenging Cards.scavenging

instance HasModifiersFor env Scavenging

ability :: AssetAttrs -> Ability
ability a = mkAbility a 1 $ ResponseAbility $ ExhaustCost (toTarget a)

instance ActionRunner env => HasAbilities env Scavenging where
  getAbilities iid (AfterPassSkillTest (Just Action.Investigate) _ who n) (Scavenging a)
    | ownedBy a iid && n >= 2 && iid == who
    = do
      hasItemInDiscard <- any (member Item . toTraits) <$> getDiscardOf iid
      cardsCanLeaveDiscard <-
        notElem CardsCannotLeaveYourDiscardPile
          <$> getModifiers (toSource a) (InvestigatorTarget iid)
      pure [ ability a | hasItemInDiscard && cardsCanLeaveDiscard ]
  getAbilities i window (Scavenging x) = getAbilities i window x

instance AssetRunner env => RunMessage env Scavenging where
  runMessage msg a@(Scavenging attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (SearchDiscard iid (InvestigatorTarget iid) [Item])
    _ -> Scavenging <$> runMessage msg attrs

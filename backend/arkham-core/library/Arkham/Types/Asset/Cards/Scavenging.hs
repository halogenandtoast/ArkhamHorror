module Arkham.Types.Asset.Cards.Scavenging
  ( Scavenging(..)
  , scavenging
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Trait

newtype Scavenging = Scavenging AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scavenging :: AssetId -> Scavenging
scavenging uuid = Scavenging $ baseAttrs uuid "01073"

instance HasModifiersFor env Scavenging where
  getModifiersFor = noModifiersFor

ability :: AssetAttrs -> Ability
ability a =
  mkAbility (toSource a) 1 (ReactionAbility $ ExhaustCost (toTarget a))

instance ActionRunner env => HasActions env Scavenging where
  getActions iid (AfterPassSkillTest (Just Action.Investigate) _ You n) (Scavenging a)
    | ownedBy a iid && n >= 2
    = do
      discard <- getDiscardOf iid
      pure
        [ ActivateCardAbilityAction iid (ability a)
        | any ((Item `member`) . getTraits) discard
        ]
  getActions i window (Scavenging x) = getActions i window x

instance AssetRunner env => RunMessage env Scavenging where
  runMessage msg a@(Scavenging attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage (SearchDiscard iid (InvestigatorTarget iid) [Item])
    _ -> Scavenging <$> runMessage msg attrs

module Arkham.Types.Asset.Cards.MagnifyingGlass1 where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype MagnifyingGlass1 = MagnifyingGlass1 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

magnifyingGlass1 :: AssetCard MagnifyingGlass1
magnifyingGlass1 = hand MagnifyingGlass1 Cards.magnifyingGlass1

instance HasModifiersFor env MagnifyingGlass1 where
  getModifiersFor _ (InvestigatorTarget iid) (MagnifyingGlass1 a) = pure
    [ toModifier a $ ActionSkillModifier Action.Investigate SkillIntellect 1
    | ownedBy a iid
    ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env MagnifyingGlass1 where
  getActions iid _ (MagnifyingGlass1 a) | ownedBy a iid = do
    locationId <- getId @LocationId iid
    clueCount' <- unClueCount <$> getCount locationId
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource a) 1 (FastAbility Free))
      | clueCount' == 0
      ]
  getActions i window (MagnifyingGlass1 x) = getActions i window x

instance (AssetRunner env) => RunMessage env MagnifyingGlass1 where
  runMessage msg a@(MagnifyingGlass1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage (ReturnToHand iid (toTarget attrs))
    _ -> MagnifyingGlass1 <$> runMessage msg attrs

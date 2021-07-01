module Arkham.Types.Asset.Cards.PoliceBadge2
  ( PoliceBadge2(..)
  , policeBadge2
  )
where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype PoliceBadge2 = PoliceBadge2 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

policeBadge2 :: AssetCard PoliceBadge2
policeBadge2 = accessory PoliceBadge2 Cards.policeBadge2

instance HasModifiersFor env PoliceBadge2 where
  getModifiersFor _ (InvestigatorTarget iid) (PoliceBadge2 a) =
    pure [ toModifier a (SkillModifier SkillWillpower 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions env PoliceBadge2 where
  getActions iid (DuringTurn InvestigatorAtYourLocation) (PoliceBadge2 a)
    | ownedBy a iid = pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource a) 1 (ActionAbility Nothing $ ActionCost 1))
      ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env PoliceBadge2 where
  runMessage msg a@(PoliceBadge2 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      activeInvestigatorId <- unActiveInvestigatorId <$> getId ()
      a <$ unshiftMessages
        [Discard (toTarget attrs), GainActions activeInvestigatorId source 2]
    _ -> PoliceBadge2 <$> runMessage msg attrs

module Arkham.Asset.Cards.PeterSylvestre (PeterSylvestre (..), peterSylvestre) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Damage
import Arkham.Matcher
import Arkham.Prelude

newtype PeterSylvestre = PeterSylvestre AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

peterSylvestre :: AssetCard PeterSylvestre
peterSylvestre = ally PeterSylvestre Cards.peterSylvestre (1, 2)

instance HasModifiersFor PeterSylvestre where
  getModifiersFor (InvestigatorTarget iid) (PeterSylvestre a) | a `controlledBy` iid = do
    pure $ toModifiers a [SkillModifier #agility 1]
  getModifiersFor _ _ = pure []

instance HasAbilities PeterSylvestre where
  getAbilities (PeterSylvestre x) =
    [ controlledAbility
        x
        1
        (exists $ HealableAsset (toSource x) HorrorType $ AssetWithId (toId x))
        (freeReaction $ TurnEnds #after You)
    ]

instance RunMessage PeterSylvestre where
  runMessage msg a@(PeterSylvestre attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ HealHorror (toTarget attrs) (toAbilitySource attrs 1) 1
      pure a
    _ -> PeterSylvestre <$> runMessage msg attrs

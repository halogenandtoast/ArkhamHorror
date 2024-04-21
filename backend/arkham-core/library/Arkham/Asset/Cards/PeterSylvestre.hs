module Arkham.Asset.Cards.PeterSylvestre (PeterSylvestre (..), peterSylvestre) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype PeterSylvestre = PeterSylvestre AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterSylvestre :: AssetCard PeterSylvestre
peterSylvestre = ally PeterSylvestre Cards.peterSylvestre (1, 2)

instance HasModifiersFor PeterSylvestre where
  getModifiersFor target (PeterSylvestre a) = modified a $ case target of
    InvestigatorTarget iid | iid `controls` a -> [SkillModifier #agility 1]
    _ -> []

instance HasAbilities PeterSylvestre where
  getAbilities (PeterSylvestre x) =
    [ controlledAbility x 1 (exists $ HealableAsset (x.ability 1) #horror $ be x)
        $ freeReaction
        $ TurnEnds #after You
    ]

instance RunMessage PeterSylvestre where
  runMessage msg a@(PeterSylvestre attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ HealHorror (toTarget attrs) (attrs.ability 1) 1
      pure a
    _ -> PeterSylvestre <$> lift (runMessage msg attrs)

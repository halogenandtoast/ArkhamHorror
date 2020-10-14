{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.CursedShores where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype CursedShores = CursedShores Attrs
  deriving newtype (Show, ToJSON, FromJSON)

cursedShores :: CursedShores
cursedShores = CursedShores $ baseAttrs
  "81007"
  "Cursed Shores"
  1
  (Static 0)
  Square
  [Plus, Triangle, Diamond, Hourglass]
  [NewOrleans, Bayou]

instance IsInvestigator investigator => HasModifiersFor env investigator CursedShores where
  getModifiersFor _ i (CursedShores Attrs {..}) = pure $ findWithDefault
    []
    (InvestigatorTarget $ getId () i)
    locationModifiersFor

instance (IsInvestigator investigator) => HasActions env investigator CursedShores where
  getActions i NonFast (CursedShores attrs@Attrs {..}) | locationRevealed = do
    baseActions <- getActions i NonFast attrs
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction
             (getId () i)
             (mkAbility (LocationSource "81007") 1 (ActionAbility 1 Nothing))
         | getId () i `elem` locationInvestigators
         ]
  getActions i window (CursedShores attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env CursedShores where
  runMessage msg (CursedShores attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessages [InvestigatorAssignDamage iid source 1 0]
      pure . CursedShores $ attrs & modifiersFor %~ insertWith
        (<>)
        (InvestigatorTarget iid)
        [AnySkillValue 2]
    SkillTestEnds -> pure . CursedShores $ attrs & modifiersFor .~ mempty
    EndRound -> pure . CursedShores $ attrs & modifiersFor .~ mempty
    _ -> CursedShores <$> runMessage msg attrs

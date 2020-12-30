{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.CursedShores where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype CursedShores = CursedShores Attrs
  deriving newtype (Show, ToJSON, FromJSON)

cursedShores :: CursedShores
cursedShores = CursedShores $ baseAttrs
  "81007"
  (LocationName "Cursed Shores" Nothing)
  EncounterSet.CurseOfTheRougarou
  1
  (Static 0)
  Square
  [Plus, Triangle, Diamond, Hourglass]
  [NewOrleans, Bayou]

instance HasModifiersFor env CursedShores where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env CursedShores where
  getActions iid NonFast (CursedShores attrs@Attrs {..}) | locationRevealed = do
    baseActions <- getActions iid NonFast attrs
    canAffordActions <- getCanAffordCost
      iid
      (toSource attrs)
      (ActionCost 1 Nothing locationTraits)
    pure
      $ baseActions
      <> [ ActivateCardAbilityAction
             iid
             (mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing))
         | iid `member` locationInvestigators && canAffordActions
         ]
  getActions i window (CursedShores attrs) = getActions i window attrs

instance LocationRunner env => RunMessage env CursedShores where
  runMessage msg l@(CursedShores attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      l <$ unshiftMessages
        [ InvestigatorAssignDamage iid source 1 0
        , CreateEffect "81007" Nothing (toSource attrs) (InvestigatorTarget iid)
        ]
    WhenEnterLocation iid lid
      | -- TODO: SHOULD WE BROADCAST LRAVING THE LOCATION INSTEAD
        lid /= locationId && iid `elem` locationInvestigators -> do
        skillCards <- map unHandCardId <$> getSetList (iid, SkillType)
        case skillCards of
          [] -> pure ()
          [x] -> unshiftMessage (DiscardCard iid x)
          xs -> unshiftMessage (chooseOne iid [ DiscardCard iid x | x <- xs ])
        CursedShores <$> runMessage msg attrs
    _ -> CursedShores <$> runMessage msg attrs

module Arkham.Types.Location.Cards.CursedShores
  ( CursedShores(..)
  , cursedShores
  )
where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype CursedShores = CursedShores LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cursedShores :: CursedShores
cursedShores = CursedShores $ baseAttrs
  "81007"
  (Name "Cursed Shores" Nothing)
  EncounterSet.CurseOfTheRougarou
  1
  (Static 0)
  Square
  [Plus, Triangle, Diamond, Hourglass]
  [NewOrleans, Bayou]

instance HasModifiersFor env CursedShores where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env CursedShores where
  getActions iid NonFast (CursedShores attrs@LocationAttrs {..}) | locationRevealed =
    withBaseActions iid NonFast attrs $ pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
      | iid `member` locationInvestigators
      ]
  getActions i window (CursedShores attrs) = getActions i window attrs

instance LocationRunner env => RunMessage env CursedShores where
  runMessage msg l@(CursedShores attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessages
      [ InvestigatorAssignDamage iid source DamageAny 1 0
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

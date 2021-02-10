module Arkham.Types.Location.Cards.TrappersCabin
  ( TrappersCabin(..)
  , trappersCabin
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype TrappersCabin = TrappersCabin LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trappersCabin :: TrappersCabin
trappersCabin = TrappersCabin $ baseAttrs
  "81014"
  (Name "Trapper's Cabin" Nothing)
  EncounterSet.CurseOfTheRougarou
  3
  (Static 0)
  Moon
  [Diamond, Moon]
  [Wilderness]

instance HasModifiersFor env TrappersCabin where
  getModifiersFor _ (InvestigatorTarget iid) (TrappersCabin attrs) =
    pure $ toModifiers
      attrs
      [ CannotGainResources | iid `member` locationInvestigators attrs ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env TrappersCabin where
  getActions iid NonFast (TrappersCabin attrs@LocationAttrs {..})
    | locationRevealed = withBaseActions iid NonFast attrs $ do
      assetNotTaken <- isNothing
        <$> getId @(Maybe StoryAssetId) (CardCode "81020")
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility
              (toSource attrs)
              1
              (ActionAbility Nothing $ Costs [ActionCost 1, ResourceCost 5])
            )
        | iid `member` locationInvestigators && assetNotTaken
        ]
  getActions i window (TrappersCabin attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env TrappersCabin where
  runMessage msg l@(TrappersCabin attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage
        (BeginSkillTest iid source (toTarget attrs) Nothing SkillIntellect 3)
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        bearTrap <- PlayerCard <$> genPlayerCard "81020"
        l <$ unshiftMessage (TakeControlOfSetAsideAsset iid bearTrap)
    _ -> TrappersCabin <$> runMessage msg attrs

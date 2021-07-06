module Arkham.Types.Location.Cards.TrappersCabin
  ( TrappersCabin(..)
  , trappersCabin
  )
where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Assets
import qualified Arkham.Location.Cards as Cards
import Arkham.PlayerCard (genPlayerCard)
import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window

newtype TrappersCabin = TrappersCabin LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trappersCabin :: LocationCard TrappersCabin
trappersCabin =
  location TrappersCabin Cards.trappersCabin 3 (Static 0) Moon [Diamond, Moon]

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
        [ UseAbility
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
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (BeginSkillTest iid source (toTarget attrs) Nothing SkillIntellect 3)
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        bearTrap <- PlayerCard <$> genPlayerCard Assets.bearTrap
        l <$ push (TakeControlOfSetAsideAsset iid bearTrap)
    _ -> TrappersCabin <$> runMessage msg attrs

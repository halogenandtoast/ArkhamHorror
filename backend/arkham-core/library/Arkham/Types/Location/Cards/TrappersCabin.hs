module Arkham.Types.Location.Cards.TrappersCabin
  ( TrappersCabin(..)
  , trappersCabin
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Assets
import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Card.PlayerCard (genPlayerCard)
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype TrappersCabin = TrappersCabin LocationAttrs
  deriving anyclass IsLocation
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

instance HasAbilities TrappersCabin where
  getAbilities (TrappersCabin attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility
          attrs
          1
          (Here <> Negate (AssetExists $ assetIs Assets.bearTrap))
        $ ActionAbility Nothing
        $ Costs [ActionCost 1, ResourceCost 5]
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env TrappersCabin where
  runMessage msg l@(TrappersCabin attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (BeginSkillTest iid source (toTarget attrs) Nothing SkillIntellect 3)
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        bearTrap <- PlayerCard <$> genPlayerCard Assets.bearTrap
        l <$ push (TakeControlOfSetAsideAsset iid bearTrap)
    _ -> TrappersCabin <$> runMessage msg attrs

module Arkham.Location.Cards.TrappersCabin
  ( TrappersCabin(..)
  , trappersCabin
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype TrappersCabin = TrappersCabin LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trappersCabin :: LocationCard TrappersCabin
trappersCabin = location TrappersCabin Cards.trappersCabin 3 (Static 0)

instance HasModifiersFor TrappersCabin where
  getModifiersFor (InvestigatorTarget iid) (TrappersCabin attrs) =
    pure $ toModifiers
      attrs
      [ CannotGainResources | iid `member` locationInvestigators attrs ]
  getModifiersFor _ _ = pure []

instance HasAbilities TrappersCabin where
  getAbilities (TrappersCabin attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility
            attrs
            1
            (Here <> Negate (AssetExists $ assetIs Assets.bearTrap))
          $ ActionAbility Nothing
          $ Costs [ActionCost 1, ResourceCost 5]
        | locationRevealed attrs
        ]

instance RunMessage TrappersCabin where
  runMessage msg l@(TrappersCabin attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> l <$ push
      (beginSkillTest iid source (toTarget attrs) SkillIntellect 3)
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        bearTrap <- PlayerCard <$> genPlayerCard Assets.bearTrap
        l <$ push (TakeControlOfSetAsideAsset iid bearTrap)
    _ -> TrappersCabin <$> runMessage msg attrs

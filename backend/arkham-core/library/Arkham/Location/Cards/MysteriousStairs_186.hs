module Arkham.Location.Cards.MysteriousStairs_186 (mysteriousStairs_186, MysteriousStairs_186 (..)) where

import Arkham.Direction
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype MysteriousStairs_186 = MysteriousStairs_186 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousStairs_186 :: LocationCard MysteriousStairs_186
mysteriousStairs_186 =
  locationWith
    MysteriousStairs_186
    Cards.mysteriousStairs_186
    3
    (Static 0)
    (connectsToL .~ setFromList [Above, Below])

instance HasModifiersFor MysteriousStairs_186 where
  getModifiersFor (InvestigatorTarget iid) (MysteriousStairs_186 attrs) = do
    here <- iid `isAt` attrs
    hasResources <- fieldSome LocationResources attrs.id
    pure
      $ toModifiers attrs
      $ guard (here && hasResources)
      *> [CannotTakeAction #move, CannotTakeAction #resign]
  getModifiersFor _ _ = pure []

instance HasAbilities MysteriousStairs_186 where
  getAbilities (MysteriousStairs_186 attrs) =
    extendRevealed
      attrs
      [ mkAbility attrs 1 $ forced $ RevealLocation #when Anyone (be attrs)
      , restrictedAbility attrs 2 Here actionAbility
      ]

instance RunMessage MysteriousStairs_186 where
  runMessage msg l@(MysteriousStairs_186 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      push $ PlaceResources (attrs.ability 1) (toTarget attrs) n
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      choices <- mins <$> traverse (traverseToSnd (`getSkillValue` iid)) [minBound .. maxBound]
      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
          [ SkillLabel skill [beginSkillTest iid (attrs.ability 2) iid skill (Fixed 2)]
          | skill <- choices
          ]
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      push $ RemoveResources (attrs.ability 2) (toTarget attrs) 1
      pure l
    _ -> MysteriousStairs_186 <$> runMessage msg attrs

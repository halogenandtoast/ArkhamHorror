module Arkham.Act.Cards.ContainingTheOutbreak (
  ContainingTheOutbreak (..),
  containingTheOutbreak,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Scenarios.WakingNightmare.Helpers

newtype ContainingTheOutbreak = ContainingTheOutbreak ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

containingTheOutbreak :: ActCard ContainingTheOutbreak
containingTheOutbreak = act (3, A) ContainingTheOutbreak Cards.containingTheOutbreak Nothing

instance HasAbilities ContainingTheOutbreak where
  getAbilities (ContainingTheOutbreak attrs) =
    [ withTooltip
        "{action}: Test {willpower} (X) to attempt to seal the rift. X is this location's shroud. Investigators at this location may spend 1 {perPlayer} clues, as a group, to automatically succeed. If you succeed, replace the damage token on this location with a horror token. For the remainder of the game, this location cannot become infested."
        $ restrictedAbility
          (proxy (LocationMatcherSource InfestedLocation) attrs)
          1
          Here
        $ ActionAbility [] (ActionCost 1 <> OptionalCost (GroupClueCost (PerPlayer 1) YourLocation))
    , restrictedAbility attrs 2 (Negate $ LocationExists InfestedLocation)
        $ Objective
        $ ForcedAbility AnyWindow
    ]

getPaidClues :: Payment -> Bool
getPaidClues (CluePayment _ _) = True
getPaidClues (Payments ps) = any getPaidClues ps
getPaidClues _ = False

instance RunMessage ContainingTheOutbreak where
  runMessage msg a@(ContainingTheOutbreak attrs) = case msg of
    UseCardAbility
      iid
      source@(ProxySource (LocationSource lid) (isSource attrs -> True))
      1
      _
      (getPaidClues -> paidClues) -> do
        pushAll
          $ [skillTestModifier source SkillTestTarget SkillTestAutomaticallySucceeds | paidClues]
          <> [ beginSkillTest
                iid
                (toAbilitySource source 1)
                iid
                #willpower
                (LocationFieldCalculation lid LocationShroud)
             ]
        pure a
    PassedThisSkillTest
      _
      source@(AbilitySource (ProxySource (LocationSource lid) (isSource attrs -> True)) 1) -> do
        pushAll [RemoveTokens source (toTarget lid) #damage 1, PlaceTokens source (toTarget lid) #horror 1]
        pure a
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      push $ advanceVia #other attrs attrs
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      joinedTheInvestigators <- getHasRecord DrMaheswaranJoinedTheInvestigation
      if joinedTheInvestigators
        then do
          resolution <- maybe R2 (const R1) <$> selectOne (assetIs Assets.drShivaniMaheswaran)
          push resolution
        else do
          push R3

      pure a
    _ -> ContainingTheOutbreak <$> runMessage msg attrs

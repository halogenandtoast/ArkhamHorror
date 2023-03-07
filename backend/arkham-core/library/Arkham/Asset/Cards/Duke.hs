module Arkham.Asset.Cards.Duke
  ( Duke(..)
  , duke
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Investigator
import Arkham.Id
import Arkham.Location.Types ( Field (..) )
import Arkham.Matcher hiding ( MoveAction )
import Arkham.Projection
import Arkham.SkillType
import Arkham.Source

newtype Duke = Duke AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

duke :: AssetCard Duke
duke = allyWith Duke Cards.duke (2, 3) (slotsL .~ mempty)

instance HasModifiersFor Duke where
  getModifiersFor (InvestigatorTarget iid) (Duke a) | controlledBy a iid = do
    mSkillTestSource <- getSkillTestSource
    case mSkillTestSource of
      Just (SkillTestSource _ _ source (Just Action.Fight))
        | isSource a source -> pure
        $ toModifiers a [BaseSkillOf SkillCombat 4, DamageDealt 1]
      Just (SkillTestSource _ _ source (Just Action.Investigate))
        | isSource a source -> pure
        $ toModifiers a [BaseSkillOf SkillIntellect 4]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities Duke where
  getAbilities (Duke a) =
    [ restrictedAbility
      a
      1
      ControlsThis
      (ActionAbility
        (Just Action.Fight)
        (Costs [ActionCost 1, ExhaustCost $ toTarget a])
      )
    , restrictedAbility
      a
      2
      ControlsThis
      (ActionAbilityWithBefore
        (Just Action.Investigate)
        (Just Action.Move)
        (Costs [ActionCost 1, ExhaustCost $ toTarget a])
      )
    ]

dukeInvestigate :: HasGame m => AssetAttrs -> InvestigatorId -> LocationId -> m Message
dukeInvestigate attrs iid lid = do
  skillType <- field LocationInvestigateSkill lid
  pure $ Investigate iid lid (toSource attrs) Nothing skillType False

instance RunMessage Duke where
  runMessage msg a@(Duke attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      a <$ push (ChooseFightEnemy iid source Nothing SkillCombat mempty False)
    UseCardAbility iid source 2 windows' _ | isSource attrs source -> do
      lid <- getJustLocation iid
      accessibleLocationIds <- traverse (traverseToSnd (dukeInvestigate attrs iid)) =<< selectList (AccessibleFrom $ LocationWithId lid)
      investigateAbilities <-
        filterM
            (andM . sequence
              [ pure . (`abilityIs` Action.Investigate)
              , (\ab -> anyM (getCanAffordAbility iid ab) windows')
                . (`applyAbilityModifiers` [ActionCostModifier (-1)])
              ]
            )
          =<< field LocationAbilities lid
      let
        investigateActions :: [UI Message] = map
          ((\f -> f windows' [])
          . AbilityLabel iid
          . (\a' -> a'
              { abilityDoesNotProvokeAttacksOfOpportunity = True
              , abilitySource = ProxySource (abilitySource a') source
              }
            )
          . (`applyAbilityModifiers` [ActionCostModifier (-1)])
          )
          investigateAbilities
      push
        $ chooseOne iid
        $ investigateActions
        <> [ targetLabel
               lid'
               [ MoveAction iid lid' Free False
               , CheckAdditionalActionCosts
                 iid
                 (LocationTarget lid')
                 Action.Investigate
                 [investigate']
               ]
           | (lid', investigate') <- accessibleLocationIds
           ]
      pure a
    UseCardAbility iid (ProxySource (LocationSource lid) (isSource attrs -> True)) 101 _ _ -> do
      investigate' <- dukeInvestigate attrs iid lid
      push investigate'
      pure a
    _ -> Duke <$> runMessage msg attrs

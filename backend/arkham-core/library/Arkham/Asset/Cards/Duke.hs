module Arkham.Asset.Cards.Duke
  ( Duke(..)
  , duke
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.InvestigatorId
import Arkham.LocationId
import Arkham.Matcher hiding (MoveAction)
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Source
import Arkham.Target

newtype Duke = Duke AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

duke :: AssetCard Duke
duke = allyWith Duke Cards.duke (2, 3) (slotsL .~ mempty)

instance HasModifiersFor env Duke where
  getModifiersFor (SkillTestSource _ _ source (Just Action.Fight)) (InvestigatorTarget iid) (Duke a)
    | controlledBy a iid && isSource a source
    = pure $ toModifiers a [BaseSkillOf SkillCombat 4, DamageDealt 1]
  getModifiersFor (SkillTestSource _ _ source (Just Action.Investigate)) (InvestigatorTarget iid) (Duke a)
    | controlledBy a iid && isSource a source
    = pure $ toModifiers a [BaseSkillOf SkillIntellect 4]
  getModifiersFor _ _ _ = pure []

instance HasAbilities Duke where
  getAbilities (Duke a) =
    [ restrictedAbility
      a
      1
      OwnsThis
      (ActionAbility
        (Just Action.Fight)
        (Costs [ActionCost 1, ExhaustCost $ toTarget a])
      )
    , restrictedAbility
      a
      2
      OwnsThis
      (ActionAbilityWithBefore
        (Just Action.Investigate)
        (Just Action.Move)
        (Costs [ActionCost 1, ExhaustCost $ toTarget a])
      )
    ]

dukeInvestigate :: AssetAttrs -> InvestigatorId -> LocationId -> Message
dukeInvestigate attrs iid lid =
  Investigate iid lid (toSource attrs) Nothing SkillIntellect False

instance AssetRunner env => RunMessage Duke where
  runMessage msg a@(Duke attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      a <$ push (ChooseFightEnemy iid source Nothing SkillCombat mempty False)
    UseCardAbility iid source windows' 2 _ | isSource attrs source -> do
      lid <- getId @LocationId iid
      investigateAbilities :: [Ability] <-
        filterM
            (andM . sequence
              [ pure . (`abilityIs` Action.Investigate)
              , getCanAffordAbility iid
                . (`applyAbilityModifiers` [ActionCostModifier (-1)])
              ]
            )
          =<< selectList (AbilityOnLocation $ LocationWithId lid)
      let
        investigateActions :: [Message] = map
          (($ windows')
          . UseAbility iid
          . (\a' -> a'
              { abilityDoesNotProvokeAttacksOfOpportunity = True
              , abilitySource = ProxySource (abilitySource a') source
              }
            )
          . (`applyAbilityModifiers` [ActionCostModifier (-1)])
          )
          investigateAbilities
      accessibleLocationIds <- selectList AccessibleLocation
      a <$ if null accessibleLocationIds
        then pushAll investigateActions
        else push
          (chooseOne iid
          $ investigateActions
          <> [ Run
                 [ MoveAction iid lid' Free False
                 , CheckAdditionalActionCosts
                   iid
                   (LocationTarget lid')
                   (toSource attrs)
                   Action.Investigate
                   [dukeInvestigate attrs iid lid']
                 ]
             | lid' <- accessibleLocationIds
             ]
          )
    _ -> Duke <$> runMessage msg attrs

module Arkham.Types.Asset.Cards.Duke
  ( Duke(..)
  , duke
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window

newtype Duke = Duke AssetAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

duke :: AssetCard Duke
duke = allyWith Duke Cards.duke (2, 3) (slotsL .~ mempty)

instance HasModifiersFor env Duke where
  getModifiersFor (SkillTestSource _ _ source _ (Just Action.Fight)) (InvestigatorTarget iid) (Duke a)
    | ownedBy a iid && isSource a source
    = pure $ toModifiers a [BaseSkillOf SkillCombat 4, DamageDealt 1]
  getModifiersFor (SkillTestSource _ _ source _ (Just Action.Investigate)) (InvestigatorTarget iid) (Duke a)
    | ownedBy a iid && isSource a source
    = pure $ toModifiers a [BaseSkillOf SkillIntellect 4]
  getModifiersFor _ _ _ = pure []

fightAbility :: AssetAttrs -> Ability
fightAbility attrs = mkAbility
  (toSource attrs)
  1
  (ActionAbility
    (Just Action.Fight)
    (Costs [ActionCost 1, ExhaustCost (toTarget attrs)])
  )

investigateAbility :: AssetAttrs -> Ability
investigateAbility attrs = mkAbility
  (toSource attrs)
  2
  (ActionAbility
    (Just Action.Investigate)
    (Costs [ActionCost 1, ExhaustCost (toTarget attrs)])
  )

instance ActionRunner env => HasActions env Duke where
  getActions iid NonFast (Duke a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid NonFast
    investigateAvailable <- hasInvestigateActions iid NonFast
    pure
      $ [ ActivateCardAbilityAction iid (fightAbility a) | fightAvailable ]
      <> [ ActivateCardAbilityAction iid (investigateAbility a)
         | investigateAvailable
         ]
  getActions i window (Duke x) = getActions i window x

dukeInvestigate :: AssetAttrs -> InvestigatorId -> LocationId -> Message
dukeInvestigate attrs iid lid =
  Investigate iid lid (toSource attrs) SkillIntellect False

instance AssetRunner env => RunMessage env Duke where
  runMessage msg a@(Duke attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      a <$ unshiftMessage (ChooseFightEnemy iid source SkillCombat mempty False)
    UseCardAbility iid source _ 2 _ | isSource attrs source -> do
      lid <- getId iid
      accessibleLocationIds <- map unAccessibleLocationId <$> getSetList lid
      a <$ if null accessibleLocationIds
        then unshiftMessage $ dukeInvestigate attrs iid lid
        else unshiftMessage
          (chooseOne iid
          $ dukeInvestigate attrs iid lid
          : [ Run
                [ MoveAction iid lid' Free False
                , dukeInvestigate attrs iid lid'
                ]
            | lid' <- accessibleLocationIds
            ]
          )
    _ -> Duke <$> runMessage msg attrs

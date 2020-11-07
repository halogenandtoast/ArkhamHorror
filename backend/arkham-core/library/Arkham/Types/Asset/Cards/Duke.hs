{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Duke
  ( Duke(..)
  , duke
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype Duke = Duke Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

duke :: AssetId -> Duke
duke uuid = Duke $ baseAttrs uuid "02014" $ do
  health ?= 2
  sanity ?= 3

instance HasModifiersFor env Duke where
  getModifiersFor (SkillTestSource _ source (Just Action.Fight)) (InvestigatorTarget iid) (Duke a)
    | ownedBy a iid && isSource a source
    = pure [BaseSkillOf SkillCombat 4, DamageDealt 1]
  getModifiersFor (SkillTestSource _ source (Just Action.Investigate)) (InvestigatorTarget iid) (Duke a)
    | ownedBy a iid && isSource a source
    = pure [BaseSkillOf SkillIntellect 4]
  getModifiersFor _ _ _ = pure []

fightAbility :: Attrs -> Ability
fightAbility attrs =
  mkAbility (toSource attrs) 1 (ActionAbility 1 (Just Action.Fight))

investigateAbility :: Attrs -> Ability
investigateAbility attrs =
  mkAbility (toSource attrs) 2 (ActionAbility 1 (Just Action.Investigate))

instance ActionRunner env => HasActions env Duke where
  getActions iid NonFast (Duke a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid NonFast
    investigateAvailable <- hasInvestigateActions iid NonFast
    pure
      $ [ ActivateCardAbilityAction iid (fightAbility a)
        | fightAvailable && not (assetExhausted a)
        ]
      <> [ ActivateCardAbilityAction iid (investigateAbility a)
         | investigateAvailable && not (assetExhausted a)
         ]
  getActions i window (Duke x) = getActions i window x

dukeInvestigate :: Attrs -> InvestigatorId -> LocationId -> Message
dukeInvestigate attrs iid lid =
  Investigate iid lid (toSource attrs) SkillIntellect mempty mempty mempty False

instance (AssetRunner env) => RunMessage env Duke where
  runMessage msg (Duke attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessage
        $ ChooseFightEnemy iid source SkillCombat mempty mempty False
      pure . Duke $ attrs & exhausted .~ True
    UseCardAbility iid source _ 2 | isSource attrs source -> do
      lid <- asks $ getId iid
      accessibleLocationIds <-
        asks $ map unAccessibleLocationId . setToList . getSet lid
      if null accessibleLocationIds
        then unshiftMessage $ dukeInvestigate attrs iid lid
        else unshiftMessage
          (chooseOne iid
          $ dukeInvestigate attrs iid lid
          : [ Run [MoveAction iid lid' False, dukeInvestigate attrs iid lid']
            | lid' <- accessibleLocationIds
            ]
          )
      pure . Duke $ attrs & exhausted .~ True
    _ -> Duke <$> runMessage msg attrs

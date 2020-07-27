{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery
  ( lookupTreachery
  , Treachery(..)
  )
where

import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.FastWindow
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import ClassyPrelude
import Data.Aeson
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Lens.Micro
import Safe (fromJustNote)

lookupTreachery :: CardCode -> (TreacheryId -> Treachery)
lookupTreachery =
  fromJustNote "Unkown treachery" . flip HashMap.lookup allTreacheries

allTreacheries :: HashMap CardCode (TreacheryId -> Treachery)
allTreacheries = HashMap.fromList
  [ ("01007", coverUp)
  , ("01162", graspingHands)
  , ("01166", ancientEvils)
  , ("01163", rottingRemains)
  , ("01164", frozenInFear)
  , ("01165", dissonantVoices)
  , ("01167", cryptChill)
  , ("01168", obscuringFog)
  ]

instance HasCardCode Treachery where
  getCardCode = treacheryCardCode . treacheryAttrs

instance HasAbilities Treachery where
  getAbilities = treacheryAbilities . treacheryAttrs

instance HasTraits Treachery where
  getTraits = treacheryTraits . treacheryAttrs

data Attrs = Attrs
  { treacheryName :: Text
  , treacheryId :: TreacheryId
  , treacheryCardCode :: CardCode
  , treacheryTraits :: HashSet Trait
  , treacheryAttachedLocation :: Maybe LocationId
  , treacheryAttachedInvestigator :: Maybe InvestigatorId
  , treacheryAbilities :: [Ability]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

abilities :: Lens' Attrs [Ability]
abilities = lens treacheryAbilities $ \m x -> m { treacheryAbilities = x }

attachedLocation :: Lens' Attrs (Maybe LocationId)
attachedLocation =
  lens treacheryAttachedLocation $ \m x -> m { treacheryAttachedLocation = x }

attachedInvestigator :: Lens' Attrs (Maybe InvestigatorId)
attachedInvestigator = lens treacheryAttachedInvestigator
  $ \m x -> m { treacheryAttachedInvestigator = x }

data Treachery
  = CoverUp CoverUpI
  | GraspingHands GraspingHandsI
  | AncientEvils AncientEvilsI
  | RottingRemains RottingRemainsI
  | FrozenInFear FrozenInFearI
  | DissonantVoices DissonantVoicesI
  | CryptChill CryptChillI
  | ObscuringFog ObscuringFogI
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

treacheryAttrs :: Treachery -> Attrs
treacheryAttrs = \case
  CoverUp (CoverUpI attrs _) -> attrs
  GraspingHands attrs -> coerce attrs
  AncientEvils attrs -> coerce attrs
  RottingRemains attrs -> coerce attrs
  FrozenInFear attrs -> coerce attrs
  DissonantVoices attrs -> coerce attrs
  CryptChill attrs -> coerce attrs
  ObscuringFog attrs -> coerce attrs

baseAttrs :: TreacheryId -> CardCode -> Attrs
baseAttrs tid cardCode =
  let
    MkEncounterCard {..} =
      fromJustNote "missing encounter card"
        $ HashMap.lookup cardCode allEncounterCards
  in
    Attrs
      { treacheryName = ecName
      , treacheryId = tid
      , treacheryCardCode = ecCardCode
      , treacheryTraits = HashSet.fromList ecTraits
      , treacheryAttachedLocation = Nothing
      , treacheryAttachedInvestigator = Nothing
      , treacheryAbilities = mempty
      }

weaknessAttrs :: TreacheryId -> CardCode -> Attrs
weaknessAttrs tid cardCode =
  let
    MkPlayerCard {..} = fromJustNote "missing weakness card"
      $ HashMap.lookup cardCode allPlayerCards
  in
    Attrs
      { treacheryName = pcName
      , treacheryId = tid
      , treacheryCardCode = pcCardCode
      , treacheryTraits = HashSet.fromList pcTraits
      , treacheryAttachedLocation = Nothing
      , treacheryAttachedInvestigator = Nothing
      , treacheryAbilities = mempty
      }

data CoverUpI = CoverUpI Attrs Int
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

coverUp :: TreacheryId -> Treachery
coverUp uuid = CoverUp $ CoverUpI
  ((weaknessAttrs uuid "01007")
    { treacheryAbilities =
      [ ( TreacherySource uuid
        , 1
        , ReactionAbility (WhenDiscoverClues You YourLocation)
        , NoLimit
        )
      ]
    }
  )
  3

newtype GraspingHandsI = GraspingHandsI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

graspingHands :: TreacheryId -> Treachery
graspingHands uuid = GraspingHands $ GraspingHandsI $ baseAttrs uuid "01162"

newtype AncientEvilsI = AncientEvilsI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ancientEvils :: TreacheryId -> Treachery
ancientEvils uuid = AncientEvils $ AncientEvilsI $ baseAttrs uuid "01166"

newtype RottingRemainsI = RottingRemainsI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

rottingRemains :: TreacheryId -> Treachery
rottingRemains uuid = RottingRemains $ RottingRemainsI $ baseAttrs uuid "01163"

newtype FrozenInFearI = FrozenInFearI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

frozenInFear :: TreacheryId -> Treachery
frozenInFear uuid = FrozenInFear $ FrozenInFearI $ baseAttrs uuid "01164"

newtype DissonantVoicesI= DissonantVoicesI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

dissonantVoices :: TreacheryId -> Treachery
dissonantVoices uuid =
  DissonantVoices $ DissonantVoicesI $ baseAttrs uuid "01165"

newtype CryptChillI = CryptChillI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

cryptChill :: TreacheryId -> Treachery
cryptChill uuid = CryptChill $ CryptChillI $ baseAttrs uuid "01167"

newtype ObscuringFogI = ObscuringFogI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

obscuringFog :: TreacheryId -> Treachery
obscuringFog uuid = ObscuringFog $ ObscuringFogI $ baseAttrs uuid "01168"

type TreacheryRunner env
  = ( HasQueue env
    , HasSet AssetId InvestigatorId env
    , HasId LocationId InvestigatorId env
    )

instance (TreacheryRunner env) => RunMessage env Treachery where
  runMessage msg = \case
    CoverUp x -> CoverUp <$> runMessage msg x
    GraspingHands x -> GraspingHands <$> runMessage msg x
    AncientEvils x -> AncientEvils <$> runMessage msg x
    RottingRemains x -> RottingRemains <$> runMessage msg x
    FrozenInFear x -> FrozenInFear <$> runMessage msg x
    DissonantVoices x -> DissonantVoices <$> runMessage msg x
    CryptChill x -> CryptChill <$> runMessage msg x
    ObscuringFog x -> ObscuringFog <$> runMessage msg x

instance (TreacheryRunner env) => RunMessage env CoverUpI where
  runMessage msg (CoverUpI attrs@Attrs {..} n) = case msg of
    RunTreachery iid tid | tid == treacheryId -> do
      unshiftMessages
        [ RemoveCardFromHand iid "01007"
        , AttachTreacheryToInvestigator tid iid
        , InvestigatorAddModifier iid (SufferTrauma 0 1 (TreacherySource tid))
        ]
      pure $ CoverUpI (attrs & attachedInvestigator ?~ iid) n
    UseCardAbility iid (TreacherySource tid, 1, _, _) | tid == treacheryId -> do
      cluesToRemove <- withQueue $ \queue -> do
        let
          (before, after) = flip break queue $ \case
            DiscoverClues{} -> True
            _ -> False
          (DiscoverClues _ _ m) = case after of
            [] -> error "DiscoverClues has to be present"
            (x : _) -> x
          remaining = case after of
            [] -> []
            (_ : xs) -> xs
        (before <> remaining, m)
      let remainingClues = max 0 (n - cluesToRemove)
      if remainingClues == 0
        then do
          unshiftMessage
            (InvestigatorRemoveAllModifiersFromSource iid (TreacherySource tid))
          pure $ CoverUpI (attrs & abilities .~ []) remainingClues
        else pure $ CoverUpI attrs remainingClues
    _ -> CoverUpI <$> runMessage msg attrs <*> pure n

instance (TreacheryRunner env) => RunMessage env GraspingHandsI where
  runMessage msg t@(GraspingHandsI attrs@Attrs {..}) = case msg of
    RunTreachery iid tid | tid == treacheryId -> t <$ unshiftMessages
      [ RevelationSkillTest iid SkillAgility 3 [] [DamagePerPointOfFailure iid]
      , DiscardTreachery tid
      ]
    _ -> GraspingHandsI <$> runMessage msg attrs

instance (TreacheryRunner env) => RunMessage env AncientEvilsI where
  runMessage msg t@(AncientEvilsI attrs@Attrs {..}) = case msg of
    RunTreachery _ tid | tid == treacheryId -> t <$ unshiftMessages
      [ PlaceDoomOnAgenda
      , AdvanceAgendaIfThresholdSatisfied
      , DiscardTreachery tid
      ]
    _ -> AncientEvilsI <$> runMessage msg attrs

instance (TreacheryRunner env) => RunMessage env RottingRemainsI where
  runMessage msg t@(RottingRemainsI attrs@Attrs {..}) = case msg of
    RunTreachery iid tid | tid == treacheryId -> t <$ unshiftMessages
      [ RevelationSkillTest
        iid
        SkillWillpower
        3
        []
        [HorrorPerPointOfFailure iid]
      , DiscardTreachery tid
      ]
    _ -> RottingRemainsI <$> runMessage msg attrs

instance (TreacheryRunner env) => RunMessage env FrozenInFearI where
  runMessage msg t@(FrozenInFearI attrs@Attrs {..}) = case msg of
    RunTreachery iid tid | tid == treacheryId -> do
      unshiftMessages
        [ AttachTreacheryToInvestigator tid iid
        , InvestigatorAddModifier
          iid
          (ActionCostOf
            (FirstOneOf [Action.Move, Action.Fight, Action.Evade])
            1
            (TreacherySource tid)
          )
        ]
      pure $ FrozenInFearI $ attrs & attachedInvestigator ?~ iid
    ChooseEndTurn iid -> t <$ unshiftMessage
      (RevelationSkillTest
        iid
        SkillWillpower
        3
        [ InvestigatorRemoveAllModifiersFromSource
          iid
          (TreacherySource treacheryId)
        , DiscardTreachery treacheryId
        ]
        []
      )
    _ -> FrozenInFearI <$> runMessage msg attrs

instance (TreacheryRunner env) => RunMessage env DissonantVoicesI where
  runMessage msg t@(DissonantVoicesI attrs@Attrs {..}) = case msg of
    RunTreachery iid tid | tid == treacheryId -> do
      unshiftMessages
        [ AttachTreacheryToInvestigator tid iid
        , InvestigatorAddModifier
          iid
          (CannotPlay [AssetType, EventType] (TreacherySource tid))
        ]
      pure $ DissonantVoicesI $ attrs & attachedInvestigator ?~ iid
    EndRound -> case treacheryAttachedInvestigator of
      Just iid -> t <$ unshiftMessages
        [ InvestigatorRemoveAllModifiersFromSource
          iid
          (TreacherySource treacheryId)
        , DiscardTreachery treacheryId
        ]
      Nothing -> pure t -- Note: This assumes the treachery never attached for some reason
    _ -> DissonantVoicesI <$> runMessage msg attrs

instance (TreacheryRunner env) => RunMessage env CryptChillI where
  runMessage msg t@(CryptChillI attrs@Attrs {..}) = case msg of
    RunTreachery iid tid | tid == treacheryId -> t <$ unshiftMessages
      [ RevelationSkillTest iid SkillWillpower 4 [] [TreacheryFailure iid tid]
      , DiscardTreachery tid
      ]
    TreacheryFailure iid tid | tid == treacheryId -> do
      assetCount <- HashSet.size <$> asks (getSet @AssetId iid)
      if assetCount > 0
        then t <$ unshiftMessage (ChooseAndDiscardAsset iid)
        else
          t <$ unshiftMessage
            (InvestigatorDamage iid (TreacherySource treacheryId) 2 0)
    _ -> CryptChillI <$> runMessage msg attrs

instance (TreacheryRunner env) => RunMessage env ObscuringFogI where
  runMessage msg t@(ObscuringFogI attrs@Attrs {..}) = case msg of
    RunTreachery iid tid | tid == treacheryId -> do
      currentLocationId <- asks (getId iid)
      unshiftMessages
        [ AttachTreacheryToLocation tid currentLocationId
        , LocationIncreaseShroud currentLocationId 2
        ]
      pure $ ObscuringFogI $ attrs & attachedLocation ?~ currentLocationId
    SuccessfulInvestigation lid | Just lid == treacheryAttachedLocation ->
      t <$ unshiftMessages
        [LocationDecreaseShroud lid 2, DiscardTreachery treacheryId]
    _ -> ObscuringFogI <$> runMessage msg attrs

instance (TreacheryRunner env) => RunMessage env Attrs where
  runMessage _msg a@Attrs {..} = pure a

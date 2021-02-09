module Arkham.Types.Act.Cards.TheChamberOfTheBeast
  ( TheChamberOfTheBeast(..)
  , theChamberOfTheBeast
  )
where


import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner

newtype TheChamberOfTheBeast = TheChamberOfTheBeast ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theChamberOfTheBeast :: TheChamberOfTheBeast
theChamberOfTheBeast = TheChamberOfTheBeast
  $ baseAttrs "02200" "The Chamber of the Beast" (Act 2 A) Nothing

instance ActionRunner env => HasActions env TheChamberOfTheBeast where
  getActions i window (TheChamberOfTheBeast x) = do
    mHiddenChamberId <- getId @(Maybe LocationId)
      (LocationWithTitle "The Hidden Chamber")
    clueCount <- maybe (pure 0) (fmap unClueCount . getCount) mHiddenChamberId
    leadInvestigatorId <- getLeadInvestigatorId
    if clueCount == 0
      then pure
        [ ActivateCardAbilityAction
            leadInvestigatorId
            (mkAbility (toSource x) 1 ForcedAbility)
        ]
      else getActions i window x

instance ActRunner env => RunMessage env TheChamberOfTheBeast where
  runMessage msg a@(TheChamberOfTheBeast attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      resolution <- maybe 3 (const 2)
        <$> getId @(Maybe StoryAssetId) (CardCode "02140")
      a <$ unshiftMessage
        (chooseOne
          leadInvestigatorId
          [ Label
              ("Resolution " <> tshow resolution)
              [ScenarioResolution $ Resolution resolution]
          ]
        )
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage (AdvanceAct actId source)
    EnemyDefeated _ _ _ "02216" _ _ ->
      a <$ unshiftMessage (ScenarioResolution $ Resolution 1)
    _ -> TheChamberOfTheBeast <$> runMessage msg attrs

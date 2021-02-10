module Arkham.Types.Treachery.Cards.PassageIntoTheVeil
  ( passageIntoTheVeil
  , PassageIntoTheVeil(..)
  )
where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Game.Helpers
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype PassageIntoTheVeil = PassageIntoTheVeil TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

passageIntoTheVeil :: TreacheryId -> a -> PassageIntoTheVeil
passageIntoTheVeil uuid _ = PassageIntoTheVeil $ baseAttrs uuid "02144"

instance HasModifiersFor env PassageIntoTheVeil where
  getModifiersFor = noModifiersFor

instance HasActions env PassageIntoTheVeil where
  getActions i window (PassageIntoTheVeil attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env PassageIntoTheVeil where
  runMessage msg t@(PassageIntoTheVeil attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      huntingHorrorAtYourLocation <- enemyAtInvestigatorLocation "02141" iid
      t <$ unshiftMessage
        (BeginSkillTest
          iid
          source
          (InvestigatorTarget iid)
          Nothing
          SkillWillpower
          (if huntingHorrorAtYourLocation then 5 else 3)
        )
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        assetIds <- getSetList @AssetId iid
        t <$ unshiftMessage
          (chooseOne
            iid
            [ Label
              "Discard the top 5 cards of your deck"
              [DiscardTopOfDeck iid 5 Nothing]
            , Label
              "Take 1 direct damage and deal 1 damage to each of your Ally assets"
              (InvestigatorDirectDamage iid source 1 0
              : [ AssetDamage aid source 1 0 | aid <- assetIds ]
              )
            ]
          )
    _ -> PassageIntoTheVeil <$> runMessage msg attrs

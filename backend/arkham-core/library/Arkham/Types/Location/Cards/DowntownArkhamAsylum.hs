{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.DowntownArkhamAsylum where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.FastWindow
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashSet as HashSet

newtype DowntownArkhamAsylum = DowntownArkhamAsylum Attrs
  deriving newtype (Show, ToJSON, FromJSON)

downtownArkhamAsylum :: DowntownArkhamAsylum
downtownArkhamAsylum =
  DowntownArkhamAsylum
    $ (baseAttrs "01131" "Downtown" 4 (PerPlayer 2) Triangle [Moon, T])
        { locationTraits = HashSet.fromList [Arkham]
        , locationVictory = Just 1
        }

instance (ActionRunner env investigator) => HasActions env investigator DowntownArkhamAsylum where
  getActions i NonFast (DowntownArkhamAsylum attrs@Attrs {..})
    | locationRevealed && getId () i `elem` locationInvestigators = do
      baseActions <- getActions i NonFast attrs
      usedAbilities <- map unUsedAbility <$> asks (getList ())
      let
        ability =
          (mkAbility (LocationSource "01131") 1 (ActionAbility 1 Nothing))
            { abilityLimit = OncePerGame
            }
      pure
        $ baseActions
        <> [ ActivateCardAbilityAction (getId () i) ability
           | (getId () i, ability) `notElem` usedAbilities
           ]
  getActions _ _ _ = pure []

instance (LocationRunner env) => RunMessage env DowntownArkhamAsylum where
  runMessage msg l@(DowntownArkhamAsylum attrs@Attrs {..}) = case msg of
    UseCardAbility iid _ (LocationSource lid) 1 | lid == locationId ->
      l <$ unshiftMessage (HealHorror (InvestigatorTarget iid) 3)
    _ -> DowntownArkhamAsylum <$> runMessage msg attrs

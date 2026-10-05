#!/usr/bin/env bash
#
# Archive the current Git tag on Zenodo.
#
# This runs from .github/workflows/zenodo-release.yml on every published
# release. It packs the tagged source tree, uploads the archive to Zenodo, and
# publishes it as a new version of an existing record (or as a new record on the
# first release).
#
# Required environment:
#   ZENODO_ACCESS_TOKEN  Zenodo personal access token. Scopes: deposit:write, deposit:actions.
#   RELEASE_TAG          Git tag of the release, for example v1.2.0.
# Optional environment:
#   ZENODO_CONCEPT_RECID Zenodo concept record id of earlier releases. Empty on the first release.
#   ZENODO_BASE_URL      Zenodo instance. Defaults to https://zenodo.org.
#   METADATA_FILE        Deposition metadata. Defaults to .zenodo.json.
set -euo pipefail

: "${ZENODO_ACCESS_TOKEN:?ZENODO_ACCESS_TOKEN is not set}"
: "${RELEASE_TAG:?RELEASE_TAG is not set}"
: "${METADATA_FILE:=.zenodo.json}"

BASE_URL="${ZENODO_BASE_URL:-https://zenodo.org}"
CONCEPT_RECID="${ZENODO_CONCEPT_RECID:-}"
VERSION="${RELEASE_TAG#v}"
ARCHIVE="planetary-health-index-${VERSION}.tar.gz"
AUTH="Authorization: Bearer ${ZENODO_ACCESS_TOKEN}"
JSON_HEADER="Content-Type: application/json"

api() {
    curl --fail-with-body --silent --show-error -H "${AUTH}" "$@"
}

echo "==> Packaging ${RELEASE_TAG}"
git archive --format=tar.gz --prefix="planetary-health-index-${VERSION}/" -o "${ARCHIVE}" HEAD
echo "    ${ARCHIVE} ($(du -h "${ARCHIVE}" | cut -f1))"

if [[ -n "${CONCEPT_RECID}" ]]; then
    echo "==> Opening a new version of concept ${CONCEPT_RECID}"
    LATEST_ID=$(api "${BASE_URL}/api/records?q=conceptrecid:${CONCEPT_RECID}&all_versions=true&sort=mostrecent&size=1" |
        jq -r '.hits.hits[0].id // empty')
    if [[ -z "${LATEST_ID}" ]]; then
        echo "No published record found for concept ${CONCEPT_RECID}" >&2
        exit 1
    fi
    DRAFT_LINK=$(api -X POST "${BASE_URL}/api/deposit/depositions/${LATEST_ID}/actions/newversion" |
        jq -r '.links.latest_draft')
    DEPOSITION=$(api "${DRAFT_LINK}")
else
    echo "==> Creating a new Zenodo record (first release)"
    DEPOSITION=$(api -X POST -H "${JSON_HEADER}" -d '{}' "${BASE_URL}/api/deposit/depositions")
fi

DEPOSITION_ID=$(jq -r '.id' <<<"${DEPOSITION}")
BUCKET=$(jq -r '.links.bucket' <<<"${DEPOSITION}")
echo "    deposition ${DEPOSITION_ID}"

# A new version inherits the files of the previous version, so clear them first.
echo "==> Replacing files"
while read -r file_id; do
    [[ -z "${file_id}" ]] && continue
    api -X DELETE "${BASE_URL}/api/deposit/depositions/${DEPOSITION_ID}/files/${file_id}" >/dev/null
done < <(api "${BASE_URL}/api/deposit/depositions/${DEPOSITION_ID}/files" | jq -r '.[].id')

api -X PUT -H "Content-Type: application/gzip" --upload-file "${ARCHIVE}" "${BUCKET}/${ARCHIVE}" >/dev/null

echo "==> Setting metadata from ${METADATA_FILE}"
METADATA=$(jq --arg version "${VERSION}" --arg date "$(date -u +%F)" \
    '. + {version: $version, publication_date: $date}' "${METADATA_FILE}")
api -X PUT -H "${JSON_HEADER}" -d "{\"metadata\": ${METADATA}}" \
    "${BASE_URL}/api/deposit/depositions/${DEPOSITION_ID}" >/dev/null

echo "==> Publishing"
RECORD=$(api -X POST "${BASE_URL}/api/deposit/depositions/${DEPOSITION_ID}/actions/publish")

echo "Version DOI: $(jq -r '.doi' <<<"${RECORD}")"
echo "Concept DOI: $(jq -r '.conceptdoi' <<<"${RECORD}")"
if [[ -z "${CONCEPT_RECID}" ]]; then
    echo "Set the repository variable ZENODO_CONCEPT_RECID to $(jq -r '.conceptrecid' <<<"${RECORD}") so later releases become new versions."
fi
